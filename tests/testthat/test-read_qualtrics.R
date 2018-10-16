context("test-read_qualtrics")

test_that("read_qualtricks works", {
  ## save test data as csv file
  tmp <- tempfile(fileext = ".csv")
  write.csv(qualtest, tmp, row.names = FALSE)
  on.exit(unlink(tmp), add = TRUE)
  d <- read_qualtrics(tmp)
  expect_true(is.data.frame(d))
  expect_equal(ncol(d), 145)
  expect_equal(nrow(d), 276)
  d <- recode_likert(d)
  expect_equal(ncol(d), 145)
  expect_equal(nrow(d), 276)
  expect_true(is.integer(d$pic_1))
})
