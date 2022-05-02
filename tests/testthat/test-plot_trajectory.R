options(warn = -1)

test_that("isn't restricted to plotting only 5 years", {
  data <- example_market_share()
  expect_true(diff(range(data$year)) > 5L)

  p <- plot_trajectory(data)
  expect_true(diff(range(p$data$year)) > 5L)
})

test_that("outputs verbatim labels", {
  data <- example_market_share()

  verbatim <- sort(unique(data$metric))
  p <- plot_trajectory(data)
  labels <- sort(unique(p$layers[[3]]$data$label))

  expect_equal(labels, verbatim)
})

test_that("outputs no title", {
  data <- example_market_share()
  p <- plot_trajectory(data)

  expect_false("title" %in% names(p$labels))
})

test_that("outputs no subtitle", {
  data <- example_market_share()
  p <- plot_trajectory(data)

  expect_false("subtitle" %in% names(p$labels))
})

test_that("outputs default axis labels", {
  data <- example_market_share()
  p <- plot_trajectory(data)

  expect_equal(p$labels$x, "year")
  expect_equal(p$labels$y, "value")
})

test_that("the errors message includes the name of the user's data", {
  # Keep even if already tested in qplot_. Non-standard evaluation is fragile
  bad_region <- head(market_share, 2L)
  bad_region$region <- c("a", "b")
  expect_error(plot_trajectory(bad_region), "bad_region")
})

test_that("By default doesn't center the Y axis", {
  data <- example_market_share()
  data_prep <- data %>%
    prep_trajectory(convert_label = identity, span_5yr = FALSE, center_y = FALSE)
  start_val <- start_value_portfolio(data_prep)

  p <- plot_trajectory(data)

  lower_y_limit <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[1]
  upper_y_limit <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[2]

  expect_false(abs(start_val - lower_y_limit) == abs(start_val - upper_y_limit))
})

test_that("Is sensitive to `center_y`", {
  data <- example_market_share()
  data_prep <- data %>%
    prep_trajectory(convert_label = identity, span_5yr = FALSE, center_y = FALSE)
  start_val <- start_value_portfolio(data_prep)

  p <- plot_trajectory(data, center_y = FALSE)
  lower_y_limit <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[1]
  upper_y_limit <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[2]

  expect_false(abs(start_val - lower_y_limit) == abs(start_val - upper_y_limit))

  p_centered <- plot_trajectory(data, center_y = TRUE)
  lower_y_limit_centered <- ggplot_build(p_centered)$layout$panel_scales_y[[1]]$range$range[1]
  upper_y_limit_centered <- ggplot_build(p_centered)$layout$panel_scales_y[[1]]$range$range[2]

  expect_true(abs(start_val - lower_y_limit_centered) == abs(start_val - upper_y_limit_centered))
})

test_that("is sensitive to `convert_label`", {
  data <- example_market_share()

  labels_def <- plot_trajectory(data) %>%
    unique_plot_data("label")
  labels_mod <- plot_trajectory(data, convert_label = toupper) %>%
    unique_plot_data("label")

  expect_false(identical(labels_def, labels_mod))
})

test_that("is sensitive to `span_5yr`", {
  data <- example_market_share()
  abort_if_year_range_is_5yr_already(data)

  p_f <- plot_trajectory(data, span_5yr = FALSE)
  expect_false(diff(year_range(p_f)) == 5)

  p_t <- plot_trajectory(data, span_5yr = TRUE)
  expect_true(diff(year_range(p_t)) == 5)
})

test_that("x-axis plots year-breaks as integers (i.e. round numbers, with no-decimals)", {
  data <- market_share %>%
    filter(
      sector == "power",
      technology == "renewablescap",
      region == "global",
      scenario_source == "demo_2020",
      between(year, 2020, 2030)
    )

  p <- plot_trajectory(data)
  g <- ggplot_build(p)
  x_axis_breaks <- g$layout$panel_params[[1]]$x$minor_breaks

  expect_true(all(x_axis_breaks - floor(x_axis_breaks) == 0))
})

test_that("is sensitive to `value_col`", {
  data <- example_market_share() %>%
    mutate(
      different_value = .data$percentage_of_initial_production_by_scope
    )

  expect_snapshot_output(plot_trajectory(data, value_col = "different_value"))
})

test_that("is sensitive to `perc_y_scale`", {
  data <- example_market_share()

  p <- plot_trajectory(data, perc_y_scale = TRUE)
  expected <- c("0%", "1%", "2%", "3%", "4%")
  actual <- ggplot_build(p)$layout$panel_params[[1]]$y$get_labels()
  expect_equal(actual, expected)

  p_no_percent <- plot_trajectory(data, perc_y_scale = FALSE)
  expected <- c("0.00", "0.01", "0.02", "0.03", "0.04")
  actual <- ggplot_build(p_no_percent)$layout$panel_params[[1]]$y$get_labels()
  expect_equal(actual, expected)
})

test_that("by default doesn't convert y-axis scale to percentage", {
  data <- example_market_share()

  p <- plot_trajectory(data)

  expected <- c("0.00", "0.01", "0.02", "0.03", "0.04")
  actual <- ggplot_build(p)$layout$panel_params[[1]]$y$get_labels()

  expect_equal(actual, expected)
})

test_that("with bad `perc_y_scale` errors gracefully", {
  data <- example_market_share()
  expect_snapshot_error(plot_trajectory(data, perc_y_scale = "bad"))
})

test_that("with 0 as extreme value plots areas correctly", {
  data <- market_share %>%
    filter(
      sector == "power",
      technology == "oilcap",
      region == "global",
      scenario_source == "demo_2020"
    )  %>%
    mutate(
      percentage_of_initial_production_by_scope = if_else(
        percentage_of_initial_production_by_scope >= 0,
        0,
        percentage_of_initial_production_by_scope
      )
    )

  p <- plot_trajectory(data)

  expect_true(min(p$data$value_low) <= min(p$data$value))

  data <- market_share %>%
    filter(
      sector == "power",
      technology == "renewablescap",
      region == "global",
      scenario_source == "demo_2020"
    )  %>%
    mutate(
      percentage_of_initial_production_by_scope = if_else(
        percentage_of_initial_production_by_scope <= 0,
        0,
        percentage_of_initial_production_by_scope
      )
    )

  p <- plot_trajectory(data)

  expect_true(max(p$data$value_high) >= max(p$data$value))
})

options(warn = 0)

test_that("throws expected warning about API change",{
  expect_snapshot_error(
    plot_trajectory(example_market_share()), class = "warning"
    )
})
