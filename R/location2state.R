#' Convert Twitter locations to U.S. state names
#'
#' Estimates U.S. state given Twitter user location data.
#'
#' @param x Character vector of Twitter user location data.
#' @return Character vector of same length with U.S. names (or a
#'   generic "United States") for values based on best guess for each
#'   user. Cases with insufficient information are returned as NAs.
#'
#' @export
location2state <- function(x) {
  ## create output vector
  states <- rep(NA_character_, length(x))
  ## trim white space
  x <- tfse::trim_ws(x)
  ## remove all periods
  x <- gsub("\\.", "", x)
  ## remove only periods between last two-letters end of string
  #dots <- grepl("[[:alpha:]]{1}\\.[[:alpha:]]\\.$", x)
  #if (sum(dots) > 0L) {
  #  x <- gsub("\\.(?=[[:alpha:]]{1}\\.)", "", x, perl = TRUE)
  #}
  ## identify trailing abbs or two-letter locations
  ## if identifies as US
  usas <- tolower(x) %in% c("us", "usa", "united states")
  states[usas] <- "United States"
  x[usas] <- ""
  endusas <- grepl("\\busa$|\\bus$", x, ignore.case = TRUE)
  states[endusas] <- "United States"
  x <- gsub("\\busa$|\\bus$", "", x)
  x[endusas] <- tfse::trim_ws(x[endusas])
  x[endusas] <- gsub("\\,$", "", x[endusas])
  ta <- grepl("[^\\,]{2,}(\\,|\\s{1})\\s{0,1}[[:alpha:]]{2}$", x)
  if (length(ta) > 0L) {
    xta <- substr(x[ta], nchar(x[ta]) - 1L, nchar(x[ta]))
    states[ta] <- state_abb2name(xta)
    x[ta] <- ""
  }
  ta <- grepl("[^\\,]{2,}\\,\\s{0,1}[^\\,]{2,}$", x)
  if (length(ta) > 0L) {
    xta <- gsub("[^\\,]{2,}\\,\\s{0,1}", "", x[ta])
    states[ta] <- state_name2name(xta)
    x[ta] <- ""
  }
  states[is.na(states)] <- state_any2name(x[is.na(states)])
  states
}

state_abb2name <- function(x) {
  datasets::state.name[match(toupper(x), datasets::state.abb)]
}

state_name2name <- function(x) {
  x <- tolower(tfse::trim_ws(x))
  datasets::state.name[match(x, tolower(datasets::state.name))]
}

state_any2name_ns <- function(x) {
  x <- tolower(tfse::trim_ws(x))
  x <- c(datasets::state.name, datasets::state.name)[match(
    x, gsub(" ", "", tolower(c(datasets::state.name, datasets::state.abb))))
  ]
  if (length(x) == 0L) return(NA_character_)
  x[1L]
}

state_any2name <- function(x) {
  x <- gsub("north ", "north", x, ignore.case = TRUE)
  x <- gsub("south ", "south", x, ignore.case = TRUE)
  x <- gsub("west ", "west", x, ignore.case = TRUE)
  x <- strsplit(x, "\\s|\\,")
  x[lengths(x) == 0L] <- NA_character_
  vapply(x, state_any2name_ns, FUN.VALUE = character(1))
}






is_country <- function(x) is.character(x) && length(1) && identical(tolower(x), "country")

is_county <- function(x) is.character(x) && length(1) && identical(tolower(x), "county")

is_state <- function(x) is.character(x) && length(1) && identical(tolower(x), "state")

prep_latlng <- function(x) {
  ## validate data frame with 2 columns
  if (is.data.frame(x) && ncol(x) < 2) {
    stop("data frame must contain 'lat' and 'lng' columns", call. = FALSE)
  }

  ## sort out lat/lng names (looks for variants)
  if (!"lat" %in% names(x) && any(grepl("^latitude$", names(x), ignore.case = TRUE))) {
    p <- grep("^latitude$", names(x), ignore.case = TRUE)[1]
    names(x)[p] <- "lat"
  }
  if (!"lat" %in% names(x) && any(grepl("^lat.*", names(x), ignore.case = TRUE))) {
    p <- grep("^lat.*", names(x), ignore.case = TRUE)[1]
    names(x)[p] <- "lat"
  }
  if (!"lng" %in% names(x) && any(grepl("^longitude$", names(x), ignore.case = TRUE))) {
    p <- grep("^longitude$", names(x), ignore.case = TRUE)[1]
    names(x)[p] <- "lng"
  }
  if (!"lng" %in% names(x) && any(grepl("^lon.*", names(x), ignore.case = TRUE))) {
    p <- grep("^lon.*", names(x), ignore.case = TRUE)[1]
    names(x)[p] <- "lng"
  }

  ## check to make sure there is lat and lng
  if (!all(c("lat", "lng") %in% names(x))) {
    stop("data frame must contain 'lat' and 'lng' columns", call. = FALSE)
  }

  ## select only lat/lng and order columns so longitude is first
  x[c("lng", "lat")]
}

ll2unit <- function(x, unit) {
  ## input should be data frame with 'lat' and 'lng' columns
  x <- prep_latlng(x)
  ## per state (plus DC, minus HI & AK)
  units <- maps::map(unit, fill = TRUE, col = "transparent", plot = FALSE)
  IDs <- sapply(strsplit(units$names, ":"), function(x) x[1])
  units_sp <- maptools::map2SpatialPolygons(
    units, IDs = IDs, proj4string = sp::CRS("+proj=longlat +datum=WGS84"))

  ## Convert x to a SpatialPoints object
  out <- character(nrow(x))
  missing_out <- is.na(x$lat) | is.na(x$lng)
  out[missing_out] <- NA_character_
  nas <- apply(x, 1, function(r) sum(is.na(r)))
  x <- x[nas == 0, ]
  pointsSP <- sp::SpatialPoints(
    x, proj4string = sp::CRS("+proj=longlat +datum=WGS84"))

  ## Use 'over' to get _indices_ of the Polygons object containing each point
  indices <- sp::over(pointsSP, units_sp)

  ## Return the state names of the Polygons object containing each point
  unitNames <- sapply(units_sp@polygons, function(x) x@ID)
  out[!missing_out] <- unitNames[indices]
  out
}

#' Latitude/longitude to state
#'
#' Converts geographical coordinates to US states.
#'
#' @param x Input data frame with lat and lng coordinate columns
#' @return Names of states in which coordinates are located
#' @export
ll2state <- function(x) UseMethod("ll2state")

#' @export
ll2state.default <- function(x) {
  if (!exists("stateMapEnv")) {
    data(stateMapEnv, package = "maps")
  }
  ll2unit(x, "state")
}

#' Latitude/longitude to county
#'
#' Converts geographical coordinates to US counties.
#'
#' @param x Input data frame with lat and lng coordinate columns
#' @return Names of counties in which coordinates are located
#' @export
ll2county <- function(x) UseMethod("ll2county")

#' @export
ll2county.default <- function(x) {
  if (!exists("countyMapEnv")) {
    data(countyMapEnv, package = "maps")
  }
  ll2unit(x, "county")
}
