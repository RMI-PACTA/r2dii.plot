test_that("inputs a data frame structured as get_example_data()", {
  raw <- get_example_data()

  no_error <- NA
  expect_error(process_input_data(raw), no_error)
})

test_that("outputs a data.frame, invisibly", {
  raw <- get_example_data()

  no_error <- NA
  expect_invisible(process_input_data(raw))
  expect_s3_class(process_input_data(raw), "data.frame")
})
