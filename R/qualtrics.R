

make_items <- function(item, context) {
  class(item) <- "item"
  attr(item, "context") <- context
  item
}

item_context <- function(item) attr(item, "context")

print.qualtrics <- function(x) {
  varnames <- names(x)
  x <- strsplit(gsub("-", "+", x), "\\+")
  is_items <- vapply(x, length, double(1)) > 1L
  if (sum(is_items, na.rm = TRUE) > 0) {
    context <- lapply(x[is_items], "[[", 1)
    item <- lapply(x[is_items], "[[", 2)
    x[is_items] <- Map(make_items, item, context)
  }
  nchars <- nchar(varnames)
  spmax <- max(nchars, na.rm = TRUE) + 2
  nspaces <- spmax - nchars
  spaste <- function(a, b) paste0(a, paste0(rep(" ", b), collapse = ""), "|")
  varnames <- unlist(Map(spaste, varnames, nspaces), use.names = FALSE)
  trunc <- function(x, n) {
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x) & any(nchar(x) > n)) {
    x[nchar(x) > n] <- paste0(substr(x[nchar(x) > n], 1, n - 3), "...")
  }
  x
  }
  n_trunc <- 76 - max(nchar(varnames), na.rm = TRUE)

  x <- lapply(x, trunc, n_trunc)
  df <- data.frame(
  varname = varnames,
  value = unlist(x),
  stringsAsFactors = FALSE)

  insert_row()
  cat("List object with ", length(x), " variables.")
  print(df, right = FALSE)
}

is_item <- function(x) inherits(x, "item")


add_items <- function(data, varnames) {
  if (sum(sapply(x, is_item), na.rm = TRUE) > 0L) {
    contexts <- lapply(x[sapply(x, is_item)], item_context)
    newcontext <- which(sapply(x, is_item))[which(!duplicated(contexts))]
    varnames <- data[[1]][newcontext]
    context_rows <- newcontext + seq(0, length(newcontext) - 1)
    contexts <- contexts[!duplicated(contexts)]
    for (i in seq_along(contexts)) {
      x <- insert_element(x, context[i], context_rows)
    }
  }
  x
}


insert_element <- function(lst, obs, n) {
  elems <- seq_len(length(lst))
  bumpelems <- lst[elems > n]
  lst[[n]] <- obs
  c(lst[1:n], bumpelems)
}

insert_row <- function(data, obs, n) {
  rows <- seq_len(nrow(data))
  bumprows <- data[rows > n, ]
  for (i in seq_len(ncol(data))) {
    x[n, i] <- obs[i]
  }
  rbind(data[1:n, ], bumprows)
}
