options(warn = -1)

test_that("if `data` is not a data frame errors gracefully", {
  expect_snapshot_error(plot_emission_intensity(1))
})

test_that("if `data` is not sda-like errors gracefully", {
  bad <- head(market_share, 1L)
  expect_snapshot_error(plot_emission_intensity(bad))
})

test_that("if `data` has zero rows errors gracefully", {
  zero_row <- prep_emission_intensity(sda)[0L, ]
  expect_snapshot_error(plot_emission_intensity(zero_row))
})

test_that("with too many sectors errors gracefully", {
  data <- head(sda, 2)
  data$sector <- c("a", "b")
  expect_snapshot_error(plot_emission_intensity(data))
})

test_that("outputs an object with no factor-columns derived from `specs`", {
  data <- prep_emission_intensity(
    head(filter(sda, sector == "cement"))
  )

  p <- plot_emission_intensity(data)
  p_data <- p$layers[[1]]$data
  has_factors <- any(unlist(lapply(p_data, is.factor)))

  expect_false(has_factors)
})

test_that("doesn't output pretty labels", {
  data <- filter(sda, sector == "cement") %>%
    prep_emission_intensity()
  p <- plot_emission_intensity(data)

  metrics <- unique(p$data$label)
  ugly <- c("projected", "corporate_economy", "target_demo", "adjusted_scenario_demo")
  expect_equal(metrics, ugly)
})

test_that("with n metrics in input outputs n lines", {
  data <- filter(sda, sector == "cement", year >= 2020, region == "global") %>%
    prep_emission_intensity()
  n_metrics <- length(unique(data$emission_factor_metric))

  n_labels <- plot_emission_intensity(data) %>%
    unique_plot_data("label") %>%
    length()

  expect_equal(n_metrics, n_labels)
})

options(warn = 0)

test_that("with data with `label` column and with `scale_colour_r2dii()`,
          outputs expected labels and colours (#535)", {

            skip_if(r_version_is_older_than(4))

            input_levels <- c(
              "projected",
              "corporate_economy",
              "target_demo",
              "adjusted_scenario_demo"
            )

            data <- filter(sda, sector == "cement", region == "global") %>%
              dplyr::mutate(
                emission_factor_metric = factor(
                  .data$emission_factor_metric,
                  levels = input_levels
                ),
                label = to_title(.data$emission_factor_metric)
              ) %>%
              prep_emission_intensity()

            input_colour_scale <- c(
              "dark_blue",
              "green",
              "grey",
              "ruby_red"
            )

            expected_output <- data.frame(
              levels = input_levels,
              colour_name = input_colour_scale
            ) %>%
              left_join(palette_colours, by = c(colour_name = "label"))

            p <- suppressWarnings(
              plot_emission_intensity(data),
              classes = "lifecycle_warning_deprecated"
            )

            p <- p + scale_colour_r2dii(
              colour_labels = input_colour_scale,
            )

            g <- ggplot_build(p)

            plot_output_labels <- g$plot$scales$scales[[3]]$get_labels()
            plot_output_colours <- g$plot$scales$scales[[3]]$palette(
              length(plot_output_labels)
            )

            plot_output <- data.frame(
              labels = plot_output_labels,
              hex = plot_output_colours
            )

            expected_output <- split(expected_output, expected_output$levels)
            plot_output <- split(plot_output, plot_output$labels)

            expect_equal(
              expected_output$projected$hex,
              plot_output$`Projected`$hex
            )

            expect_equal(
              expected_output$corporate_economy$hex,
              plot_output$`Corporate Economy`$hex
            )

            expect_equal(
              expected_output$target_demo$hex,
              plot_output$`Target Demo`$hex
            )

            expect_equal(
              expected_output$adjusted_scenario_demo$hex,
              plot_output$`Adjusted Scenario Demo`$hex
            )

          })

test_that("with `convert_label = to_title`, outputs custom colour scale with
          expected order (#536)", {

  skip_if(r_version_is_older_than(4))

  input_levels <- c(
    "projected",
    "corporate_economy",
    "target_demo",
    "adjusted_scenario_demo"
  )

  data <- filter(sda, sector == "cement", region == "global") %>%
    dplyr::mutate(
      emission_factor_metric = factor(
        .data$emission_factor_metric,
        levels = input_levels
      )
    ) %>%
    prep_emission_intensity(convert_label = to_title)

  input_colour_scale <- c(
    "dark_blue",
    "green",
    "grey",
    "ruby_red"
  )

  expected_output <- data.frame(
    levels = input_levels,
    colour_name = input_colour_scale
  ) %>%
    left_join(palette_colours, by = c(colour_name = "label"))


  p <- suppressWarnings(
    plot_emission_intensity(data),
    classes = "lifecycle_warning_deprecated"
  )

  p <- p + scale_colour_r2dii(
    colour_labels = input_colour_scale,
  )

  g <- ggplot_build(p)

  plot_output_labels <- g$plot$scales$scales[[3]]$get_labels()
  plot_output_colours <- g$plot$scales$scales[[3]]$palette(
    length(plot_output_labels)
  )

  plot_output <- data.frame(
    labels = plot_output_labels,
    hex = plot_output_colours
  )

  expected_output <- split(expected_output, expected_output$levels)
  plot_output <- split(plot_output, plot_output$labels)

  expect_equal(
    expected_output$projected$hex,
    plot_output$`Projected`$hex
  )

  expect_equal(
    expected_output$corporate_economy$hex,
    plot_output$`Corporate Economy`$hex
  )

  expect_equal(
    expected_output$target_demo$hex,
    plot_output$`Target Demo`$hex
  )

  expect_equal(
    expected_output$adjusted_scenario_demo$hex,
    plot_output$`Adjusted Scenario Demo`$hex
  )


})

test_that("with too many lines to plot errors gracefully", {
  data <- filter(sda, sector == "cement") %>%
    bind_fake_sda_metrics(8) %>%
    prep_emission_intensity()

  expect_snapshot_error(plot_emission_intensity(data))
})
