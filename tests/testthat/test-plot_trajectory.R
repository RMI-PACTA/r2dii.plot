options(warn = -1)

test_that("isn't restricted to plotting only 5 years", {
  data <- prep_trajectory(example_market_share())
  expect_true(diff(range(data$year)) > 5L)

  p <- plot_trajectory(data)
  expect_true(diff(range(p$data$year)) > 5L)
})

test_that("outputs verbatim labels", {
  data <- example_market_share()
  prepped_data <- prep_trajectory(example_market_share())

  verbatim <- sort(unique(data$metric))
  p <- plot_trajectory(prepped_data)
  labels <- sort(unique(p$layers[[3]]$data$label))

  expect_equal(labels, verbatim)
})

test_that("outputs no title", {
  data <- prep_trajectory(example_market_share())
  p <- plot_trajectory(data)

  expect_false("title" %in% names(p$labels))
})

test_that("outputs no subtitle", {
  data <- prep_trajectory(example_market_share())
  p <- plot_trajectory(data)

  expect_false("subtitle" %in% names(p$labels))
})

test_that("outputs default axis labels", {
  data <- prep_trajectory(example_market_share())
  p <- plot_trajectory(data)

  expect_equal(p$labels$x, "year")
  expect_equal(p$labels$y, "value")
})

test_that("the errors message includes the name of the user's data", {
  # Keep even if already tested in qplot_. Non-standard evaluation is fragile
  bad_region <- head(market_share, 2L) %>%
    mutate(region = c("a", "b")) %>%
    prep_trajectory()

  expect_error(plot_trajectory(bad_region), "bad_region")
})

test_that("By default doesn't center the Y axis", {
  data <- example_market_share() %>%
    prep_trajectory(convert_label = identity, span_5yr = FALSE)
  start_val <- start_value_portfolio(data)

  p <- plot_trajectory(data, center_y = FALSE)

  lower_y_limit <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[1]
  upper_y_limit <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[2]

  expect_false(abs(start_val - lower_y_limit) == abs(start_val - upper_y_limit))
})

test_that("x-axis plots year-breaks as integers (i.e. round numbers, with no-decimals)", {
  data <- market_share %>%
    filter(
      sector == "power",
      technology == "renewablescap",
      region == "global",
      scenario_source == "demo_2020",
      between(year, 2020, 2030)
    ) %>%
    prep_trajectory()

  p <- plot_trajectory(data)
  g <- ggplot_build(p)
  x_axis_breaks <- g$layout$panel_params[[1]]$x$minor_breaks

  expect_true(all(x_axis_breaks - floor(x_axis_breaks) == 0))
})

test_that("is sensitive to `perc_y_scale`", {
  data <- prep_trajectory(example_market_share())

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
  data <- prep_trajectory(example_market_share())

  p <- plot_trajectory(data)

  expected <- c("0.00", "0.01", "0.02", "0.03", "0.04")
  actual <- ggplot_build(p)$layout$panel_params[[1]]$y$get_labels()

  expect_equal(actual, expected)
})

test_that("with bad `perc_y_scale` errors gracefully", {
  data <- prep_trajectory(example_market_share())
  expect_snapshot_error(plot_trajectory(data, perc_y_scale = "bad"))
})

test_that("with 0 as extreme value plots areas correctly", {
  data <- market_share %>%
    filter(
      sector == "power",
      technology == "oilcap",
      region == "global",
      scenario_source == "demo_2020"
    ) %>%
    mutate(
      percentage_of_initial_production_by_scope = if_else(
        percentage_of_initial_production_by_scope >= 0,
        0,
        percentage_of_initial_production_by_scope
      )
    ) %>%
    prep_trajectory()

  p <- plot_trajectory(data)

  expect_true(min(p$data$value_low) <= min(p$data$value))

  data <- market_share %>%
    filter(
      sector == "power",
      technology == "renewablescap",
      region == "global",
      scenario_source == "demo_2020"
    ) %>%
    mutate(
      percentage_of_initial_production_by_scope = if_else(
        percentage_of_initial_production_by_scope <= 0,
        0,
        percentage_of_initial_production_by_scope
      )
    ) %>%
    prep_trajectory()

  p <- plot_trajectory(data)

  expect_true(max(p$data$value_high) >= max(p$data$value))
})

# FIXME
# test_that("handles center_y correctly", {
#   data <- prep_trajectory(example_market_share())
#   p <- plot_trajectory(data, center_y = TRUE)
#   expect_equal(
#     abs(min(ggplot_build(p)$data[[1]]$y)),
#     abs(max(ggplot_build(p)$data[[1]]$y))
#     )
# })
