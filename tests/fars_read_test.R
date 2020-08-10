library(testthat)
#' test function fars_read
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  tibble::as_tibble(data)
}
data<-fars_read("accident_2013.csv.bz2")
expect_that(data,is_a("data.frame"))
expect_that(fars_read("accident_2020.csv.bz2"),throws_error())
