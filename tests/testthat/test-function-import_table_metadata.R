#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("import_table_metadata() works", {

  expect_error(
    kwb.readxl:::import_table_metadata()
    # argument "file" is missing, with no default
  )

})

