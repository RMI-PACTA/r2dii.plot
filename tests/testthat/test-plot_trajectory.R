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

test_that("with zero-row data errors gracefully", {
  zero_row <- market_share[0L, ]
  expect_snapshot_error(plot_trajectory(zero_row))
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

test_that("with too many scenarios errors gracefully", {
  add_fake_scenarios_market_share <- function(data, n) {
    sector <- data$sector[1]
    technology <- data$technology[1]
    region <- data$region[1]
    scenario_source <- data$scenario_source[1]
    min_year <- min(data$year)
    max_year <- max(data$year)
    for (i in 1:n) {
      fake_data <- tibble(
        sector = rep(sector, 2),
        technology = rep(technology, 2),
        region = rep(region, 2),
        scenario_source = rep(scenario_source, 2),
        year = c(min_year, max_year),
        metric = glue("target_{letters[i]}"),
        production = 100,
        technology_share = 0.1
      )

      data <- rbind(data, fake_data)
    }
    data
  }
  data <- subset(
    market_share,
    sector == "power" &
      region == "global" &
      technology == "renewablescap" &
      year <= 2025
  ) %>%
    add_fake_scenarios_market_share(5)

  expect_snapshot_error(plot_trajectory(data))
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

test_that("informs that values are normalized", {
  data <- example_market_share()
  op <- options(r2dii.plot.quiet = FALSE)
  on.exit(op, add = TRUE)
  expect_message(plot_trajectory(data), "[Nn]ormalizing")
})

test_that("works with example data", {
  data <- subset(
    market_share,
    sector == "power" &
      region == "global" &
      technology == "renewablescap" &
      year <= 2025
  )

  expect_no_error(plot_trajectory(data))
})

test_that("works with input data starting before start year of 'projected'", {
  data <- subset(
    market_share,
    sector == "power" &
      region == "global" &
      technology == "renewablescap" &
      year <= 2025
  )
  start_year <- min(subset(data, metric == "projected")$year)
  early_row <- tibble(
    sector =  "power",
    technology = "renewablescap",
    year = start_year - 1,
    region = "global",
    scenario_source = "demo_2020",
    metric = "corporate_economy",
    production = 1,
    technology_share = 0.1
  )
  data <- data %>%
    rbind(early_row)
  expect_no_error(plot_trajectory(data))
})
