test_that("outputs a data.frame", {
  expect_s3_class(get_example_data(), "data.frame")
})
