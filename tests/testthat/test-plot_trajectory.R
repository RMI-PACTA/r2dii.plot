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

  labels <- if ("get_labs" %in% getNamespaceExports("ggplot2")) {
    ggplot2::get_labs(p)
  } else {
    p$labels
  }

  expect_equal(labels$x, "year")
  expect_equal(labels$y, "value")
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

test_that("handles center_y correctly", {
  data <- prep_trajectory(example_market_share())
  p <- plot_trajectory(data, center_y = TRUE)
  out <- ggplot_build(p)$data
  label_groups <- as.data.frame(out[[3]])
  label_groups <- select(label_groups, label, group)
  out <- as.data.frame(out[[1]])
  out <- left_join(out, label_groups, by = "group")

  out <- dplyr::filter(out, x == min(.data$x))
  out <- dplyr::filter(out, label %in% c("projected", "corporate_economy"))

  expect_equal(abs(min(out$y)), abs(max(out$y)))
})
