test_data <- subset(sda_demo, sector == "cement" & region == "global")

test_that("returns a data frame", {
  result <- prep_emission_intensity(test_data)
  expect_true(is.data.frame(result))
})

test_that("converts year to Date type", {
  result <- prep_emission_intensity(test_data)
  expect_true(all(sapply(result$year, class) == "Date"))
})

test_that("handles span_5yr correctly", {
  result <- prep_emission_intensity(test_data, span_5yr = TRUE)
  expect_true(all(as.integer(format(result$year, "%Y")) <= min(test_data$year) + 5))
})

test_that("if `data` has zero rows errors gracefully", {
  zero_row <- sda_demo[0L, ]
  expect_snapshot_error(prep_emission_intensity(zero_row))
})

# Test handling of convert_label for factor and non-factor labels
test_that("handles convert_label correctly", {
  # Mock convert_label function for testing
  mock_convert_label <- function(x) paste0("Converted_", x)

  # Test with factor labels
  test_data_factor <- mutate(test_data, label = emission_factor_metric)
  test_data_factor$label <- factor(test_data_factor$label)
  result <- prep_emission_intensity(test_data_factor, convert_label = mock_convert_label)
  expect_true(all(grepl("Converted_", result$label)))

  # Test with non-factor labels
  test_data_nonfactor <- mutate(test_data, label = emission_factor_metric)
  result <- prep_emission_intensity(test_data_nonfactor, convert_label = mock_convert_label)
  expect_true(all(grepl("Converted_", result$label)))
})

test_that("columns in output match what is documented in `data_dictionary`", {
  out <- prep_emission_intensity(test_data)

  data_dict <- dplyr::filter(data_dictionary, dataset == "prep_emission_intensity_output")

  expect_setequal(names(out), data_dict[["column"]])
  expect_mapequal(sapply(out, typeof), setNames(data_dict[["typeof"]], data_dict[["column"]]))
})
