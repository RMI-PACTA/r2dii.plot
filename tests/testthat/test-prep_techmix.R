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

test_that("with zero-row data errors gracefully", {
  zero_row <- market_share[0L, ]

  expect_snapshot_error(
    prep_techmix(zero_row)
  )
})

test_that("without `market_share` data errors gracefully", {
  bad_kind <- filter(sda, sector == first(sector))

  expect_snapshot_error(prep_techmix(bad_kind))
})

test_that("with more than one scenario errors gracefully", {
  prep <- example_market_share()
  expect_snapshot_error(prep_techmix(prep))
})

test_that("with too many sectors errors gracefully", {
  bad_sector <- head(market_share, 2L)
  bad_sector$sector <- c("a", "b")
  expect_snapshot_error(prep_techmix(bad_sector))
})

test_that("with too many regions errors gracefully", {
  bad_region <- head(market_share, 2L)
  bad_region$region <- c("a", "b")
  expect_snapshot_error(prep_techmix(bad_region))
})

test_that("with too many scenario_source errors gracefully", {
  bad_scenario_source <- head(market_share, 2L)
  bad_scenario_source$scenario_source <- c("a", "b")
  expect_snapshot_error(prep_techmix(bad_scenario_source))
})

test_that("with too few scenarios errors gracefully", {
  too_few <- head(market_share, 2L)
  too_few$metric <- c(main_line(), "corporate_economy")
  expect_snapshot_error(prep_techmix(too_few))
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

test_that("is sensitive to `span_5yr`", {
  data <- market_share %>%
    filter(
      scenario_source == "demo_2020",
      sector == "power",
      region == "global",
      metric %in% c("projected", "corporate_economy", "target_sds")
    )
  abort_if_year_range_is_5yr_already(data)

  p_f <- prep_techmix(data, span_5yr = FALSE)
  expect_false(abs(max(p_f$year) - min(p_f$year)) == 5)

  p_t <- prep_techmix(data, span_5yr = TRUE)
  expect_true(abs(max(p_t$year) - min(p_t$year)) == 5)
})

test_that("is sensitive to `convert_tech_label`", {
  data <- market_share %>%
    filter(
      year %in% c(2020, 2025),
      scenario_source == "demo_2020",
      sector == "power",
      region == "global",
      metric %in% c("projected", "corporate_economy", "target_sds")
    )

  labels_def <- data %>%
    prep_techmix() %>%
    pull("label_tech")

  labels_mod <- data %>%
    prep_techmix(convert_tech_label = toupper) %>%
    pull("label_tech")

  expect_false(identical(labels_def, labels_mod))
})

test_that("is sensitive to `convert_label`", {
  data <- market_share %>%
    filter(
      year %in% c(2020, 2025),
      scenario_source == "demo_2020",
      sector == "power",
      region == "global",
      metric %in% c("projected", "corporate_economy", "target_sds")
    )

  labels_def <- prep_techmix(data) %>%
    pull("label")
  labels_mod <- prep_techmix(data, convert_label = toupper) %>%
    pull("label")

  expect_false(identical(labels_def, labels_mod))
})

test_that("with input data before start year of 'projected' prep_techmix
          outputs data with start year of 'projected'", {
            data <- filter(
              market_share,
              sector == "power",
              region == "global",
              year <= 2025,
              metric %in% c("projected", "corporate_economy", "target_sds")
            )
            start_year <- min(filter(data, metric == "projected")$year, na.rm = TRUE)
            early_row <- tibble(
              sector = "power",
              technology = "renewablescap",
              year = start_year - 1,
              region = "global",
              scenario_source = "demo_2020",
              metric = "corporate_economy",
              production = 1,
              technology_share = 0.1
            )
            data <- data %>%
              bind_rows(early_row)

            expect_equal(min(prep_techmix(data)$year, na.rm = TRUE), start_year)
          })

test_that("input with no `projected` value errors gracefully", {
  bad_data <- filter(
    test_data,
    metric != "projected"
  )
  expect_error(prep_techmix(bad_data), class = "no_projected")
})
