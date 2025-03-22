test_data <- subset(
  market_share_demo,
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

test_that("handles span_5yr correctly", {
  out <- prep_trajectory(example_market_share(), span_5yr = TRUE)
  expect_true(all(out$year <= min(out$year) + 5))
})

test_that("the errors message includes the name of the user's data", {
  # Keep even if already tested in qplot_. Non-standard evaluation is fragile
  bad_region <- head(market_share_demo, 2L) %>%
    mutate(region = c("a", "b"))

  expect_error(prep_trajectory(bad_region), "bad_region")
})

test_that("columns in output match what is documented in `data_dictionary`", {
  out <- prep_trajectory(test_data)

  data_dict <- dplyr::filter(data_dictionary, dataset == "prep_trajectory_output")

  expect_setequal(names(out), data_dict[["column"]])
  expect_mapequal(sapply(out, typeof), setNames(data_dict[["typeof"]], data_dict[["column"]]))
})
