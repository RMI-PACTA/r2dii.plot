options(warn = -1)

test_that("if `data` is not a data frame errors gracefully", {
  expect_snapshot_error(plot_emission_intensity(1))
})

test_that("if `data` is not sda-like errors gracefully", {
  bad <- head(market_share, 1L)
  expect_snapshot_error(plot_emission_intensity(bad))
})

test_that("if `data` has zero rows errors gracefully", {
  zero_row <- sda[0L, ]
  expect_snapshot_error(plot_emission_intensity(zero_row))
})

test_that("with too many sectors errors gracefully", {
  data <- head(sda, 2)
  data$sector <- c("a", "b")
  expect_snapshot_error(plot_emission_intensity(data))
})

test_that("outputs an object with no factor-columns derived from `specs`", {
  data <- head(filter(sda, sector == "cement"))

  p <- plot_emission_intensity(data)
  p_data <- p$layers[[1]]$data
  has_factors <- any(unlist(lapply(p_data, is.factor)))

  expect_false(has_factors)
})

test_that("doesn't output pretty labels", {
  data <- filter(sda, sector == "cement")
  p <- plot_emission_intensity(data)

  metrics <- unique(p$data$label)
  ugly <- c("projected", "corporate_economy", "target_demo", "adjusted_scenario_demo")
  expect_equal(metrics, ugly)
})

test_that("with too many lines to plot errors gracefully", {
  data <- filter(sda, sector == "cement") %>%
    bind_fake_sda_metrics(8)
  expect_snapshot_error(plot_emission_intensity(data))
})

test_that("is sensitive to `convert_label`", {
  data <- filter(sda, sector == "cement")

  labels_def <- plot_emission_intensity(data) %>%
    unique_plot_data("label")
  labels_mod <- plot_emission_intensity(data, convert_label = toupper) %>%
    unique_plot_data("label")

  expect_false(identical(labels_def, labels_mod))
})

test_that("is sensitive to `span_5yr`", {
  data <- filter(sda, sector == "cement")
  abort_if_year_range_is_5yr_already(data)

  p_f <- plot_emission_intensity(data, span_5yr = FALSE)
  expect_false(diff(year_range(p_f)) == 5)

  p_t <- plot_emission_intensity(data, span_5yr = TRUE)
  expect_true(diff(year_range(p_t)) == 5)
})

test_that("with n metrics in input outputs n lines", {
  data <- filter(sda, sector == "cement", year >= 2020, region == "global")
  n_metrics <- length(unique(data$emission_factor_metric))

  n_labels <- plot_emission_intensity(data) %>%
    unique_plot_data("label") %>%
    length()

  expect_equal(n_metrics, n_labels)
})

options(warn = 0)

test_that("throws expected warning about API change", {
  data <- head(filter(sda, sector == "cement"))
  expect_snapshot_error(
    plot_emission_intensity(data),
    class = "warning"
  )
})

test_that("works well with `scale_colour_r2dii`", {
  #TODO: Check if this belongs here or in `test-scale_colour_r2dii.R`
  data <- filter(sda, sector == "cement", region == "global")

  input_levels <- c(
    "projected",
    "corporate_economy",
    "target_demo",
    "adjusted_scenario_demo"
  )

  input_color_scale <- c(
    "dark_blue",
    "green",
    "grey",
    "ruby_red"
  )

  input_color_scale_hex <- data.frame(label = input_color_scale) %>%
    left_join(palette_colours, by = "label") %>%
    pull(hex)

  expected_output <- data.frame(
    levels = input_levels,
    hex = input_color_scale_hex
  )

  data <- data %>%
    dplyr::mutate(
      emission_factor_metric = factor(
        .data$emission_factor_metric,
        levels = input_levels
      )
    )

  p <- plot_emission_intensity(data)
  p <- p + scale_colour_r2dii(
      labels = input_color_scale
    )

  # print the levels that colours are applied to
  ordered_output_levels <- levels(match_lines_order(p$data))

  # print the actual colour scales of the plot
  ordered_output_colour_scale <- p$scales$get_scales("colour")$palette(
    length(ordered_output_levels)
  )

  plot_output <- data.frame(
    levels = ordered_output_levels,
    hex = ordered_output_colour_scale
  )

  out <- left_join(
    plot_output,
    expected_output,
    by = "levels",
    suffix = c("_out", "_expected")
  ) %>%
    split(.$levels)

  expect_equal(out$projected$hex_out, out$projected$hex_expected)
  expect_equal(out$corporate_economy$hex_out, out$corporate_economy$hex_expected)
  expect_equal(out$target_demo$hex_out, out$target_demo$hex_expected)
  expect_equal(out$adjusted_scenario_demo$hex_out, out$adjusted_scenario_demo$hex_expected)

})
