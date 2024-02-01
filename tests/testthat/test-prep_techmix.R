test_data <- subset(
  market_share,
  scenario_source == "demo_2020" &
    sector == "power" &
    region == "global" &
    metric %in% c("projected", "corporate_economy", "target_sds")
)

# Test that it returns a data frame
test_that("prep_techmix returns a data frame", {
  result <- prep_techmix(test_data)
  expect_true(is.data.frame(result))
})

# Test for correct processing of span_5yr
test_that("prep_techmix handles span_5yr correctly", {
  result <- prep_techmix(test_data, span_5yr = TRUE)
  expect_true(all(result$year <= min(test_data$year) + 5))
})

# Test handling of convert_label and convert_tech_label
test_that("prep_techmix handles convert_label and convert_tech_label correctly", {
  mock_convert_label <- function(x) paste0("Converted_", x)
  mock_convert_tech_label <- function(x) tolower(x)

  test_data <- mutate(test_data, label = metric)
  test_data <- mutate(test_data, label_tech = technology)

  result <- prep_techmix(test_data, convert_label = mock_convert_label, convert_tech_label = mock_convert_tech_label)
  expect_true(all(grepl("Converted_", result$label)))
  expect_equal(setdiff(unique(result$label_tech), unique(test_data$label_tech)), character(0))
})

# Test for correct filtering of years
test_that("prep_techmix filters years correctly", {
  # Assuming function filters for start and future years
  start_year <- min(test_data$year)
  future_year <- max(test_data$year)
  result <- prep_techmix(test_data)
  expect_true(all(result$year %in% c(start_year, future_year)))
})
