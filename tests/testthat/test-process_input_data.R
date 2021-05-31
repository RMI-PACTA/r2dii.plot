test_that("inputs a data frame structured as market_share", {
  expect_no_error(process_input_data(fake_market_share()))
})

test_that("outputs a visible data frame", {
  expect_s3_class(process_input_data(fake_market_share()), "data.frame")
  expect_visible(process_input_data(fake_market_share()))
})

test_that("adds a column `metric_type`", {
  before <- fake_market_share()
  expect_false(hasName(before, "metric_type"))

  after <- process_input_data(before)
  expect_true(hasName(after, "metric_type"))
})

test_that("depends on input column `metric`", {
  missing_metric <- select(fake_market_share(), -metric)
  expect_snapshot_error(process_input_data(missing_metric))
})

test_that("modifies `metric`", {
  before <- market_share
  after <- process_input_data(before)
  expect_false(identical(before$metric, after$metric))
})

test_that("handles values of `metric`", {
  # FIXME: If metric has none of the expected values, Should we throw an error?
  expect_no_error(
    process_input_data(bad <- fake_market_share(metric = "bad metric"))
  )
})
