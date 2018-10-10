##mwk
##read in mturk/qualtrics data

#' read in functions
source("funs.R")

#' read in data
d <- read_qualtrics_csv(list.files("../data/", full.names = TRUE))

#' aggregate items
d$socdom <- rowMeans(d[, grep("socdom", names(d))], na.rm = TRUE)
d$ambiv_sexism <- rowMeans(d[, grep("ambiv_sexism", names(d))], na.rm = TRUE)
d$intentions_djt <- rowMeans(d[, grep("intentions_djt", names(d))], na.rm = TRUE)
d$polar_therm <- abs(d$therm_1 - d$therm_2)

#' vote hillary
d$hillary <- d$vote_2016_choice == 1

#' vote trump
d$trump <- d$vote_2016_choice == 2

#' regression models
summary(lm(trump ~ ambiv_sexism + socdom + sex + age + edu, d))

#' summarize demographic data
sum_stats(d[, 137:151])

