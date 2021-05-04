test_that("inputs a data frame structured as get_example_data()", {
  expect_no_error(process_input_data(fake_data()))
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

test_that("depends on input column `metric`", {
  missing_metric <- select(fake_data(), -metric)
  expect_error(
    class = "missing_names",
    process_input_data(missing_metric)
  )
})

test_that("modifies `metric`", {
  before <- get_example_data()
  after <- process_input_data(before)
  expect_false(identical(before$metric, after$metric))
})

test_that("handles values of `metric`", {
  # FIXME: If metric has none of the expected values, Should we throw an error?
  expect_no_error(
    process_input_data(bad <- fake_data(metric = "bad metric"))
  )
})
