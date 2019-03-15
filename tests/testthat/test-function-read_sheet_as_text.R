library(testthat)

test_that("read_sheet_as_text() works", {

  fun <- kwb.readxl:::read_sheet_as_text
  file <- system.file("extdata", "example.xlsx", package = "kwb.readxl")
  
  expect_error(fun())
  expect_error(fun(file = "xyz", sheet = "abc"))
  expect_error(fun(file, sheet = "abc"))
  expect_error(fun(file, c("Baseline", "HRUA")))
  
  check_result <- function(x) {
    expect_is(x, "matrix")
    expect_true(is.character(x))
  }
  
  check_result(fun(file, "Baseline"))
  check_result(fun(file, "HRUA"))
})
