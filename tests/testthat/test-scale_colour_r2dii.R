test_that("outputs a gg ScaleDiscrete", {
  expect_s3_class(scale_colour_r2dii(), c("gg"))
  expect_s3_class(scale_colour_r2dii(), c("ScaleDiscrete"))
  expect_s3_class(scale_fill_r2dii(), c("gg"))
  expect_s3_class(scale_fill_r2dii(), c("ScaleDiscrete"))
})

test_that("if with bad `labels` errors gracefully", {
  expect_snapshot_error(scale_colour_r2dii(colour_labels = c("bad")))
  expect_snapshot_error(scale_fill_r2dii(colour_labels = c("bad")))
})

test_that("changes the plot colours as expected", {
  p <- ggplot(ggplot2::mpg) +
    geom_point(aes(x = displ, y = hwy, colour = class))

  colours_default <- unique_data1(p, "colour")
  colours_changed <- unique_data1(p + scale_colour_r2dii(), "colour")

  expect_false(identical(colours_default, colours_changed))
})

test_that("changes the plot fill as expected", {
  p <- ggplot(ggplot2::mpg) +
    geom_histogram(aes(x = cyl, fill = class), position = "dodge", bins = 5)

  colours_default <- unique_data1(p, "fill")
  colours_changed <- unique_data1(p + scale_fill_r2dii(), "fill")

  expect_false(identical(colours_default, colours_changed))
})

test_that("with data having specific level factors, scales colours as expected
          (#527)", {

  skip_if(r_version_is_older_than(4))
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

  data <- prep_emission_intensity(data)

  p <- suppressWarnings(
    plot_emission_intensity(data),
    classes = "lifecycle_warning_deprecated"
  )
  p <- p + scale_colour_r2dii(
     colour_labels = input_color_scale,
     labels = input_levels
  )

  # print the levels that colours are applied to
  ordered_output_levels <- levels(p$data$emission_factor_metric)

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
