#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("get_tables_from_sheet() works", {

  expect_error(
    kwb.readxl:::get_tables_from_sheet()
    # argument "text_sheet" is missing, with no default
  )

})

