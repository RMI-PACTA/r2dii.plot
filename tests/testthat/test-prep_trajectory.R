test_data <- subset(
  market_share,
  sector == "power" &
    technology == "renewablescap" &
    region == "global" &
    scenario_source == "demo_2020"
)

test_that("returns a data frame", {
  result <- prep_trajectory(test_data)
  expect_true(is.data.frame(result))
})

test_that("returns expected columns", {
  expected_cols <- c("year", "metric", "label", "technology", "value", "sector")
  result <- prep_trajectory(test_data)
  expect_equal(
    setdiff(expected_cols, colnames(result)),
    character()
  )
})

test_that("handles span_5yr correctly", {
  result <- prep_trajectory(test_data, span_5yr = TRUE)
  expect_true(all(result$year <= min(test_data$year) + 5))
})

test_that("handles center_y correctly", {
  normal_result <- prep_trajectory(test_data, center_y = FALSE)

  expect_equal(abs(min(result$value_low)), abs(max(result$value)))
})

test_that("handles value_col correctly", {
# FIXME: There's no way to test what value of `value_col` has been used, just by
# looking at the output.
})
