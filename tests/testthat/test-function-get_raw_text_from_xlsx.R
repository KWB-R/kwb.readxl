test_that("get_raw_text_from_xlsx() works", {

  fun <- kwb.readxl::get_raw_text_from_xlsx
  file <- system.file("extdata", "example.xlsx", package = "kwb.readxl")
  
  expect_error(fun())
  
  result <- fun(file)
  
  expect_is(result, "list")
  expect_identical(names(result), c("sheet_01", "sheet_02"))
  expect_true(all(sapply(result, is.matrix)))
  expect_true(all(sapply(result, is.character)))
  sheet_info <- kwb.utils::getAttribute(result, "sheet_info")
  expect_identical(dim(sheet_info), c(2L, 2L))
  expect_identical(names(sheet_info), c("sheet_id", "sheet_name"))
  expect_identical(sheet_info$sheet_id, c("sheet_01", "sheet_02"))
  expect_identical(sheet_info$sheet_name, c("Baseline", "HRUA"))
})
