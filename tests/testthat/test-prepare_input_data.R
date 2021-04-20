test_that("inputs a data frame structured as get_example_data()", {
  no_error <- NA
  expect_error(process_input_data(fake_data()), no_error)
})

test_that("outputs a visible data frame", {
  expect_s3_class(process_input_data(fake_data()), "data.frame")
  expect_visible(process_input_data(fake_data()))
})

test_that("adds a column `metric_type`", {
  before <- fake_data()
  expect_false(hasName(before, "metric_type"))

  after <- process_input_data(before)
  expect_true(hasName(after, "metric_type"))
})

# FIXME: The helpfile shoud likely not expose implementation details. That will
# too easily become obsolete when the implementation changes. Instead you may
# better "document" this expectation with an informative error message.
test_that("depends on input column `metric`", {
  bad <- fake_data()
  bad$metric <- NULL

  # FIXME: The error message could be more graceful
  message <- "nms %in% .* are not all TRUE"
  expect_error(process_input_data(bad), message)
})

test_that("modifies `metric`", {
  before <- get_example_data()
  after <- process_input_data(before)
  expect_false(identical(before$metric, after$metric))
})

test_that("handles values of `metric`", {
  bad <- fake_data(metric = "bad metric")

  # FIXME: If metric has none of the expected values, Should we throw an error?
  expect_no_error(process_input_data(bad))
})
