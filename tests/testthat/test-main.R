context("main")

test_that("get_raw_text_from_xlsx() works as expected", {
  
  file <- system.file("extdata", "example.xlsx", package = "kwb.readxl")

  text_sheets <<- kwb.readxl:::get_raw_text_from_xlsx(file = file)
  
  expect_named(text_sheets)
  
  expect_true(all(grepl("^sheet_", names(text_sheets))))
  
  sheet_info <- attr(text_sheets, "sheet_info")
  
  expect_false(is.null(sheet_info))
  
  expect_true(all(c("sheet_id", "sheet_name") %in% names(sheet_info)))
})

test_that("split_into_tables() works as expected", {
  
  tables_list <- list(
    kwb.readxl:::split_into_tables(text_sheets$sheet_01),
    kwb.readxl:::split_into_tables(text_sheets$sheet_02)
  )
  
  for (tables in tables_list) {
    
    expect_named(tables)
    expect_true(all(grepl("^table_", names(tables))))
    
    table_info <- attr(tables, "table_info")
    
    expect_false(is.null(table_info))
    
    columns <- c(
      "table_id", "table_name", "first_row", "last_row", "first_col",
      "last_col", "n_headers"
    )
    
    expect_true(all(columns %in% names(table_info)))
  }
})
