#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("get_text_tables_from_xlsx() works", {

  expect_error(
    kwb.readxl:::get_text_tables_from_xlsx()
    # argument "file" is missing, with no default
  )

})

