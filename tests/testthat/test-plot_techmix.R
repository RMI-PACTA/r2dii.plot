options(warn = -1)

test_that("without a `data` frame errors gracefully", {
  expect_error(plot_techmix(1), "data.frame.*not")
})

test_that("outputs a ggplot", {
  data <- example_tech_mix() %>%
    prep_techmix()
  p <- plot_techmix(data)
  expect_s3_class(p, "ggplot")
})

test_that("with missing crucial names errors gracefully", {
  data <- head(market_share_demo)

  bad <- select(data, -metric)
  expect_error(class = "hint_missing_names", plot_techmix(bad))

  bad <- select(data, -technology_share)
  expect_error(class = "hint_missing_names", plot_techmix(bad))

  bad <- select(data, -year)
  expect_error(class = "hint_missing_names", plot_techmix(bad))

  bad <- select(data, -scenario_source)
  expect_error(class = "hint_missing_names", plot_techmix(bad))
})

test_that("informs that extreme years are used", {
  mydata <- filter(
    market_share_demo,
    sector == "power",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  ) %>%
    prep_techmix()

  restore <- options(r2dii.plot.quiet = FALSE)
  expect_snapshot(invisible(
    plot_techmix(mydata)
  ))
  options(restore)
})

test_that("does not modify `metric`", {
  data <- filter(
    market_share_demo,
    sector == "power",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  )
  metrics <- sort(unique(data$metric))

  p <- plot_techmix(prep_techmix(data))
  out <- sort(as.character(unique(p$data$metric)))
  expect_equal(out, metrics)
})

test_that("Outputs no title", {
  data <- filter(
    market_share_demo,
    sector == "power",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  ) %>%
    prep_techmix()

  p <- plot_techmix(data)

  expect_false("title" %in% names(p$labels))
})

test_that("Does not output pretty labels", {
  data <- filter(
    market_share_demo,
    sector == "power",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  ) %>%
    prep_techmix()

  p <- plot_techmix(data)

  metrics <- sort(unique(p$data$label))
  ugly <- sort(c("projected", "target_sds", "corporate_economy"))
  expect_equal(metrics, ugly)
})

test_that("Doesn't output pretty legend labels", {
  data <- filter(
    market_share_demo,
    sector == "power",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  ) %>%
    prep_techmix()

  p <- plot_techmix(data)

  metrics <- unique(p$data$label_tech)
  ugly <- c("coalcap", "gascap", "hydrocap")
  expect_equal(metrics[1:3], ugly)

  data <- filter(
    market_share_demo,
    sector == "automotive",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  ) %>%
    prep_techmix()

  p <- plot_techmix(data)

  metrics <- unique(p$data$label_tech)
  ugly <- c("electric", "hybrid", "ice", "fuelcell")
  expect_equal(metrics, ugly)
})

test_that("When data has 'label_tech' it is used in the plot", {
  data <- filter(
    market_share_demo,
    sector == "automotive",
    region == "global",
    year <= 2025,
    metric %in% c("projected", "corporate_economy", "target_sds")
  ) %>%
    mutate(label_tech = case_when(
      technology == "ice" ~ "My custom label",
      TRUE ~ .data$technology
    )) %>%
    prep_techmix()

  p <- plot_techmix(data)

  expect_true("My custom label" %in% unique(p$data$label_tech))
})

test_that("With random order of data ouputs plot with labels in the right order", {
  data <- market_share_demo %>%
    filter(
      year %in% c(2020, 2025),
      scenario_source == "demo_2020",
      sector == "power",
      region == "global",
      metric %in% c("projected", "corporate_economy", "target_sds")
    ) %>%
    mutate(metric = factor(
      .data$metric,
      levels = c("corporate_economy", "projected", "target_sds")
    )) %>%
    arrange(.data$metric) %>%
    mutate(metric = as.character(.data$metric)) %>%
    prep_techmix()

  p <- plot_techmix(data)

  right_order <- c("target_sds", "corporate_economy", "projected")
  names(right_order) <- right_order
  expect_equal(p$plot_env$labels, right_order)
})

test_that("with no scenario for start year of 'projected' doesn't plot scenario bar", {
  data <- market_share_demo %>%
    filter(
      year %in% c(2020, 2025),
      scenario_source == "demo_2020",
      sector == "power",
      region == "global",
      metric %in% c("projected", "corporate_economy", "target_sds")
    )

  data_no_scenario_start_year <- data %>%
    filter(
      !((metric == "target_sds") & (year == 2020))
    ) %>%
    prep_techmix()

  expect_snapshot_output(plot_techmix(data_no_scenario_start_year))
})
