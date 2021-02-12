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

test_that("adds a column `metric_type`", {
  before <- get_example_data()
  expect_false(hasName(before, "metric_type"))

  after <- process_input_data(before)
  expect_true(hasName(after, "metric_type"))
})
