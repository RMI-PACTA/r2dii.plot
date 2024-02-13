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

test_that("handles value_col correctly", {

  test_data_dif_value_col <- test_data %>%
    rename(new_column = percentage_of_initial_production_by_scope)

  result <- prep_trajectory(test_data)
  result_dif_col <- prep_trajectory(test_data_dif_value_col, value_col = "new_column")

  expect_equal(
    setdiff(names(result), names(result_dif_col)),
    "percentage_of_initial_production_by_scope"
    )

  expect_equal(
    setdiff(names(result_dif_col), names(result)),
    "new_column"
  )

})
