#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("get_bounding_boxes() works", {

  expect_error(
    kwb.readxl:::get_bounding_boxes()
    # argument "cell_ids" is missing, with no default
  )

})

