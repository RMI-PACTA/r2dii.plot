test_that("without a `data` frame errors gracefully", {
  expect_error(plot_techmix(1), "data.frame.*not")
})

test_that("without `market_share` data errors gracefully", {
  bad_kind <- filter(sda, sector == first(sector))
  expect_snapshot_error(plot_techmix(bad_kind))
})

test_that("with zero-row data errors gracefully", {
  zero_row <- market_share[0L, ]
  expect_snapshot_error(
    plot_techmix(zero_row)
  )
})

test_that("with more than one scenario errors gracefully", {
  prep <- example_market_share()
  expect_snapshot_error(plot_techmix(prep))
})

test_that("with too many sectors errors gracefully", {
  bad_sector <- head(market_share, 2L)
  bad_sector$sector <- c("a", "b")
  expect_snapshot_error(plot_techmix(bad_sector))
})

test_that("with too many regions errors gracefully", {
  bad_region <- head(market_share, 2L)
  bad_region$region <- c("a", "b")
  expect_snapshot_error(plot_techmix(bad_region))
})

test_that("with too many scenario_source errors gracefully", {
  bad_scenario_source <- head(market_share, 2L)
  bad_scenario_source$scenario_source <- c("a", "b")
  expect_snapshot_error(plot_techmix(bad_scenario_source))
})

test_that("with too few scenarios errors gracefully", {
  too_few <- head(market_share, 2L)
  too_few$metric <- c(main_line(), "corporate_economy")
  expect_snapshot_error(plot_techmix(too_few))
})

test_that("outputs a ggplot", {
  data <- head(market_share, 3)
  p <- plot_techmix(data)
  expect_s3_class(p, "ggplot")
})

test_that("with missing crucial names errors gracefully", {
  data <- head(market_share)

  bad <- select(data, -metric)
  expect_error(class = "hint_missing_names", plot_techmix(bad))

  bad <- select(data, -technology_share)
  expect_error(class = "hint_missing_names", plot_techmix(bad))

  bad <- select(data, -year)
  expect_error(class = "hint_missing_names", plot_techmix(bad))

  bad <- select(data, -scenario_source)
  expect_error(class = "hint_missing_names", plot_techmix(bad))
})

test_that("with input data before start year of 'projected' prep_techmix
          outputs data with start year of 'projected'", {
  data <- filter(
    market_share,
    sector == "power",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  )
  start_year <- min(filter(data, metric == "projected")$year)
  early_row <- tibble(
    sector = "power",
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
  expect_equal(min(prep_techmix(data)$year), start_year)
})

test_that("informs that extreme years are used", {
  data <- filter(
    market_share,
    sector == "power",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  )

  restore <- options(r2dii.plot.quiet = FALSE)
  expect_snapshot(invisible(
    plot_techmix(data)
  ))
  options(restore)
})

test_that("does not modify `metric`", {
  data <- filter(
    market_share,
    sector == "power",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  )
  metrics <- sort(unique(data$metric))

  p <- plot_techmix(data)
  out <- sort(as.character(unique(p$data$metric)))
  expect_equal(out, metrics)
})

test_that("Outputs no title", {
  data <- filter(
    market_share,
    sector == "power",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  )
  p <- plot_techmix(data)

  expect_false("title" %in% names(p$labels))
})

test_that("Does not output pretty labels", {
  data <- filter(
    market_share,
    sector == "power",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  )
  p <- plot_techmix(data)

  metrics <- unique(p$data$label)
  ugly <- c("projected", "corporate_economy", "target_sds")
  expect_equal(metrics, ugly)
})

test_that("Doesn't output pretty legend labels", {
  data <- filter(
    market_share,
    sector == "power",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  )
  p <- plot_techmix(data)

  metrics <- unique(p$data$label_tech)
  ugly <- c("coalcap", "gascap", "hydrocap")
  expect_equal(metrics[1:3], ugly)

  data <- filter(
    market_share,
    sector == "automotive",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  )
  p <- plot_techmix(data)

  metrics <- unique(p$data$label_tech)
  ugly <- c("electric", "hybrid", "ice")
  expect_equal(metrics, ugly)
})

test_that("When data has 'label_tech' it is used in the plot", {
  data <- filter(
    market_share,
    sector == "automotive",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  ) %>%
  mutate(label_tech = case_when(
    technology == "ice" ~ "My custom label",
    TRUE ~ .data$technology
  ))
  p <- plot_techmix(data)

  expect_true("My custom label" %in% unique(p$data$label_tech))
})

test_that("With random order of data ouputs plot with labels in the right order",{
  data <- market_share %>%
  filter(
    year %in% c(2020, 2025),
    scenario_source == "demo_2020",
    sector == "power",
    region == "global",
    metric %in% c("projected", "corporate_economy", "target_sds")
  ) %>%
  mutate(metric = factor(
    .data$metric, levels = c("corporate_economy","projected","target_sds"))) %>%
  arrange(.data$metric) %>%
  mutate(metric = as.character(.data$metric))
  p <- plot_techmix(data)

  right_order <- c("target_sds", "corporate_economy", "projected")
  expect_equal(p$plot_env$labels, right_order)
})
