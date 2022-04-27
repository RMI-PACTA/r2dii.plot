test_that("outputs a data.frame", {
  out <- example_market_share() %>% prep_trajectory()
  expect_s3_class(out, "data.frame")
})

test_that("the errors message includes the name of the user's data", {
  # Keep even if already tested in qplot_. Non-standard evaluation is fragile
  bad_region <- head(market_share, 2L)
  bad_region$region <- c("a", "b")
  expect_error(prep_trajectory(bad_region), "bad_region")
})

test_that("with bad input parameters errors gracefully", {
  bad <- "bad"
  data <- example_market_share()

  expect_snapshot_error(data %>% prep_trajectory(convert_label = bad))
  expect_snapshot_error(data %>% prep_trajectory(span_5yr = bad))
  expect_snapshot_error(data %>% prep_trajectory(value_col = bad))
})

test_that("is sensitive to `convert_label`", {
  data <- example_market_share()

  labels_def <- prep_trajectory(data, convert_label = identity) %>%
    pull(.data$label)
  labels_mod <- prep_trajectory(data, convert_label = toupper) %>%
    pull(.data$label)

  expect_false(identical(labels_def, labels_mod))
})

test_that("is sensitive to `span_5yr`", {
  data <- example_market_share()
  abort_if_year_range_is_5yr_already(data)

  year_span <- function(data) {
    max(data$year, na.rm = TRUE) - min(data$year, na.rm = TRUE)
  }

  data_no5yr <- prep_trajectory(data, span_5yr = FALSE)
  expect_false(diff(range(data_no5yr$year)) == 5)

  data_5yr <- prep_trajectory(data, span_5yr = TRUE)
  expect_true(diff(range(data_5yr$year)) == 5)
})

test_that("is sensitive to `value_col`", {
  data_default <- example_market_share() %>%
    prep_trajectory()

  data_check <- example_market_share() %>%
    mutate(
      different_value = .data$percentage_of_initial_production_by_scope + 5
    ) %>%
    prep_trajectory(value_col = "different_value")

  expect_false(identical(data_default$value, data_check$value))
})
