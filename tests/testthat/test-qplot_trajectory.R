test_that("works with up to 4 scenarios (+ 1 portfolio + 1 benchmark)", {
  data <- example_market_share()
  count_metrics <- function(p) length(unique(p$layers[[2]]$data$metric))

  n <- 3L
  prep <- filter(data, metric %in% unique(metric)[1:n])
  p <- qplot_trajectory(prep)
  expect_equal(count_metrics(p), n)

  n <- 4L
  prep <- filter(data, metric %in% unique(metric)[1:n])
  p <- qplot_trajectory(prep)
  expect_equal(count_metrics(p), n)

  n <- 5L
  prep <- filter(data, metric %in% unique(metric)[1:n])
  p <- qplot_trajectory(prep)
  expect_equal(count_metrics(p), n)

  n <- 6L
  prep <- filter(data, metric %in% unique(metric)[1:n])
  # Faking a new scenario to reach the maximum number of scenarios we support
  xyz <- filter(prep, metric == "target_sds")
  xyz$metric <- xyz$metric <- "target_xyz"
  xyz <- bind_rows(prep, xyz)
  p <- qplot_trajectory(xyz)
  expect_equal(count_metrics(p), n)
})

test_that("if `data` is not a data frame errors gracefully", {
  expect_snapshot_error(qplot_trajectory(1))
})

test_that("if `data` is not market_share-like errors gracefully", {
  bad <- head(sda)
  expect_snapshot_error(qplot_trajectory(bad))
})

test_that("with zero-row data errors gracefully", {
  zero_row <- market_share[0L, ]
  expect_snapshot_error(qplot_trajectory(zero_row))
})

test_that("with too many sectors errors gracefully", {
  bad_sector <- head(market_share, 2L)
  bad_sector$sector <- c("a", "b")
  expect_snapshot_error(qplot_trajectory(bad_sector))
})

test_that("with too many technologies errors gracefully", {
  bad_tech <- head(market_share, 2L)
  bad_tech$technology <- c("a", "b")
  expect_snapshot_error(qplot_trajectory(bad_tech))
})

test_that("with too many regions errors gracefully", {
  bad_region <- head(market_share, 2L)
  bad_region$region <- c("a", "b")
  expect_snapshot_error(qplot_trajectory(bad_region))
})

test_that("with too many scenario_source errors gracefully", {
  bad_scenario_source <- head(market_share, 2L)
  bad_scenario_source$scenario_source <- c("a", "b")
  expect_snapshot_error(qplot_trajectory(bad_scenario_source))
})

test_that("with too many scenarios errors gracefully", {
  data <- example_market_share() %>%
    bind_fake_market_share_metrics(n = 5, prefix = "target_")

  expect_snapshot_error(qplot_trajectory(data))
})

test_that("with too many metrics errors gracefully", {
  data <- example_market_share() %>%
    bind_fake_market_share_metrics(n = 6)

  expect_snapshot_error(qplot_trajectory(data))
})

test_that("with missing crucial names errors gracefully", {
  data <- head(market_share)

  bad <- select(data, -metric)
  expect_error(class = "hint_missing_names", qplot_trajectory(bad))

  bad <- select(data, -sector)
  expect_error(class = "hint_missing_names", qplot_trajectory(bad))

  bad <- select(data, -technology)
  expect_error(class = "hint_missing_names", qplot_trajectory(bad))

  bad <- select(data, -region)
  expect_error(class = "hint_missing_names", qplot_trajectory(bad))

  bad <- select(data, -year)
  expect_error(class = "hint_missing_names", qplot_trajectory(bad))

  bad <- select(data, -scenario_source)
  expect_error(class = "hint_missing_names", qplot_trajectory(bad))

  bad <- select(data, -production)
  expect_error(class = "hint_missing_names", qplot_trajectory(bad))
})

test_that("works with brown technology", {
  brown <- "oil"
  data <- filter(market_share, technology == brown, region == first(region))
  expect_warning(
    regexp = NA,
    qplot_trajectory(data)
  )
})

test_that("works with input data starting before start year of 'projected'", {
  data <- example_market_share()
  start_year <- min(filter(data, metric == "projected")$year, na.rm = TRUE)
  to_exclude <- data %>%
    fake_market_share(year = start_year - 1, metric = "corporate_economy")
  data <- data %>%
    bind_rows(to_exclude)
  expect_no_error(plot_trajectory(data))
})

test_that("informs that values are normalized", {
  data <- example_market_share()

  restore <- options(r2dii.plot.quiet = FALSE)
  expect_snapshot(invisible(
    qplot_trajectory(data)
  ))
  options(restore)
})

test_that("informs if excluding data before start year of 'projected'", {
  data <- example_market_share()
  start_year <- min(filter(data, metric == "projected")$year, na.rm = TRUE)
  to_exclude <- data %>%
    fake_market_share(year = start_year - 1, metric = "corporate_economy")

  restore <- options(r2dii.plot.quiet = FALSE)
  data %>%
    bind_rows(to_exclude) %>%
    qplot_trajectory() %>%
    expect_message("[Nn]ormalizing") %>%
    expect_message("[Rr]emoving")
  options(restore)
})

test_that("with no data to remove does not inform about removing rows", {
  restore <- options(r2dii.plot.quiet = FALSE)
  example_market_share() %>%
    qplot_trajectory() %>%
    expect_message("[Nn]ormalizing") %>%
    # Irrelevant message
    expect_no_message() # No other message should bubble up
  options(restore)
})

test_that("'metric' in plot data is a factor with 'projected' as last element", {
  data <- example_market_share()
  plot <- qplot_trajectory(data)
  expect_equal(levels(plot$data$metric)[nlevels(plot$data$metric)], "projected")

  brown <- "oil"
  data <- filter(market_share, technology == brown, region == first(region))
  plot <- qplot_trajectory(data)
  expect_equal(levels(plot$data$metric)[nlevels(plot$data$metric)], "projected")
})

test_that("does not modify `metric`", {
  data <- example_market_share()
  metrics <- sort(unique(data$metric))

  p <- qplot_trajectory(data)
  out <- sort(as.character(unique(p$layers[[2]]$data$metric)))
  expect_equal(out, metrics)
})

test_that("Plots a data set with maximum time horizon of 5 years", {
  data <- example_market_share()
  p <- qplot_trajectory(data)
  expect_true(diff(year_range(p)) <= 5)
})

test_that("Outputs pretty labels", {
  data <- example_market_share()
  p <- qplot_trajectory(data)

  metrics <- unique(p$layers[[3]]$data$label)
  pretty <- c("CPS", "SPS", "SDS", "Corporate Economy", "Projected")
  expect_equal(metrics, pretty)
})

test_that("Wraps the title as expected", {
  data <- example_market_share()
  p <- qplot_trajectory(data)

  expect_snapshot_output(p$labels$title)
})

test_that("Wraps the subtitle as expected", {
  data <- example_market_share()
  p <- qplot_trajectory(data)

  expect_snapshot_output(p$labels$subtitle)
})

test_that("Prints axis labels as expected", {
  data <- example_market_share()
  p <- qplot_trajectory(data)

  expect_equal(p$labels$x, "Year")
  expect_snapshot_output(p$labels$x)

  expect_match(p$labels$y, "[Pp]roduction [Rr]ate.*normalized.*")
  expect_snapshot_output(p$labels$y)
})

test_that("the errors message includes the name of the user's data", {
  # Keep even if already tested in qplot_. Non-standard evaluation is fragile
  bad_region <- head(market_share, 2L)
  bad_region$region <- c("a", "b")
  expect_error(qplot_trajectory(bad_region), "bad_region")
})

test_that("By defeault centers the Y axis", {
  data <- example_market_share()
  data_prep <- data %>%
    prep_trajectory(convert_label = identity, span_5yr = FALSE, center_y = TRUE)
  start_val <- start_value_portfolio(data_prep)

  p <- qplot_trajectory(data)

  expect_equal(
    abs(start_val - ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[1]),
    abs(start_val - ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[2])
  )
})
