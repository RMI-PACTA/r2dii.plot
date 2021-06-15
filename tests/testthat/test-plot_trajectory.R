test_that("outputs the expected ggplot object", {
  mauro <- path.expand("~") == "/home/mauro"
  skip_if_not(mauro, message = "Brittle test meant to run on mauro's pc only")

  p <- plot_trajectory(example_market_share())
  p$plot_env <- NULL

  expect_snapshot(str(p))
})

test_that("works with up to 4 scenarios (+ 1 portfolio + 1 benchmark)", {
  data <- example_market_share()
  count_metrics <- function(p) length(unique(p$layers[[2]]$data$metric))

  n <- 3L
  prep <- filter(data, metric %in% unique(metric)[1:n])
  p <- plot_trajectory(prep)
  expect_equal(count_metrics(p), n)

  n <- 4L
  prep <- filter(data, metric %in% unique(metric)[1:n])
  p <- plot_trajectory(prep)
  expect_equal(count_metrics(p), n)

  n <- 5L
  prep <- filter(data, metric %in% unique(metric)[1:n])
  p <- plot_trajectory(prep)
  expect_equal(count_metrics(p), n)

  n <- 6L
  prep <- filter(data, metric %in% unique(metric)[1:n])
  # Faking a new scenario to reach the maximum number of scenarios we support
  xyz <- filter(prep, metric == "target_sds")
  xyz$metric <- xyz$metric <- "target_xyz"
  xyz <- rbind(prep, xyz)
  p <- plot_trajectory(xyz)
  expect_equal(count_metrics(p), n)
})

test_that("with corrupt `scenario_colours` errors gracefully", {
  prep <- example_market_share()

  too_short <- 4L
  corrupt <- head(scenario_colours, too_short)
  op <- options("r2dii.plot.scenario_colours" = corrupt)
  on.exit(options(op), add = TRUE)

  expect_snapshot_error(plot_trajectory(prep))
})

test_that("if `data` is not a data frame errors gracefully", {
  expect_snapshot_error(plot_trajectory(1))
})

test_that("if `data` is not market_share-like errors gracefully", {
  bad <- head(sda)
  expect_snapshot_error(plot_trajectory(bad))
})

test_that("with cero-row data errors gracefully", {
  cero_row <- market_share[0L, ]
  expect_snapshot_error(plot_trajectory(cero_row))
})

test_that("with too many sectors errors gracefully", {
  bad_sector <- head(market_share, 2L)
  bad_sector$sector <- c("a", "b")
  expect_snapshot_error(plot_trajectory(bad_sector))
})

test_that("with too many technologies errors gracefully", {
  bad_tech <- head(market_share, 2L)
  bad_tech$technology <- c("a", "b")
  expect_snapshot_error(plot_trajectory(bad_tech))
})

test_that("with too many regions errors gracefully", {
  bad_region <- head(market_share, 2L)
  bad_region$region <- c("a", "b")
  expect_snapshot_error(plot_trajectory(bad_region))
})

test_that("with too many scenario_source errors gracefully", {
  bad_scenario_source <- head(market_share, 2L)
  bad_scenario_source$scenario_source <- c("a", "b")
  expect_snapshot_error(plot_trajectory(bad_scenario_source))
})

test_that("with inexistent `main_line` errors gracefully", {
  data <- head(market_share, 1L)
  expect_snapshot_error(plot_trajectory(data, main_line = "bad"))
})

test_that("with too many scenarios errors gracefully", {
  data <- head(market_share, 7)
  data$metric <- c(
    "projected", "corporate_economy", glue("target_{letters[1:5]}")
  )
  expect_snapshot_error(plot_trajectory(data))
})

test_that("is sensitive to `main_line`", {
  data <- example_market_share()
  plot_trajectory(data, main_line = "corporate_economy")
  expect_no_error(plot_trajectory(data, main_line = "corporate_economy"))
})

test_that("with too long `main_line` errors gracefully", {
  data <- head(market_share, 1L)
  expect_snapshot_error(plot_trajectory(data, main_line = c("too", "long")))
})

test_that("is sensitive to `normalize`", {
  data <- filter(market_share, technology == first(technology))
  pull_value <- function(p) p$layers[[1]]$data$value

  expect_false(
    identical(
      pull_value(plot_trajectory(data, normalize = TRUE)),
      pull_value(plot_trajectory(data, normalize = FALSE))
    )
  )
})

test_that("with missing crucial names errors gracefully", {
  data <- head(market_share)

  bad <- select(data, -metric)
  expect_error(class = "hint_missing_names", plot_trajectory(bad))

  bad <- select(data, -sector)
  expect_error(class = "hint_missing_names", plot_trajectory(bad))

  bad <- select(data, -technology)
  expect_error(class = "hint_missing_names", plot_trajectory(bad))

  bad <- select(data, -region)
  expect_error(class = "hint_missing_names", plot_trajectory(bad))

  bad <- select(data, -year)
  expect_error(class = "missing_names", prep_trajectory(bad))

  bad <- select(data, -scenario_source)
  expect_error(class = "missing_names", prep_trajectory(bad))

  bad <- select(data, -production)
  expect_error(class = "missing_names", prep_trajectory(bad))
})

test_that("works with brown technology", {
  brown <- "oil"
  data <- filter(market_share, technology == brown, region == first(region))
  expect_warning(
    regexp = NA,
    plot_trajectory(data)
  )
})
