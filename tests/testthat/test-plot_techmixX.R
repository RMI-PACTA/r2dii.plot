test_that("outputs the expected ggplot object", {
  mauro <- path.expand("~") == "/home/mauro"
  skip_if_not(mauro, message = "Brittle test meant to run on mauro's pc only")

  # data <- head(filter(market_share, sector == "cement"))
  data <- market_share %>%
    filter(
      sector == first(.data$sector),
      metric %in% c("projected", "corporate_economy", "target_sds")
    )
  p <- plot_techmixX(data)
  p$plot_env <- NULL
  expect_snapshot(str(p))
})

test_that("without a `data` frame errors gracefully", {
  expect_error(plot_techmixX(1), "data.frame.*not")
})

test_that("without `market_share` data errors gracefully", {
  bad_kind <- filter(sda, sector == first(sector))
  expect_snapshot_error(plot_techmixX(bad_kind))
})

test_that("with cero-row data errors gracefully", {
  cero_row <- market_share[0L, ]
  expect_snapshot_error(
    plot_techmixX(cero_row)
  )
})

test_that("with too many sectors errors gracefully", {
  bad_sector <- head(market_share, 2L)
  bad_sector$sector <- c("a", "b")
  expect_snapshot_error(plot_techmixX(bad_sector))
})

test_that("with too many regions errors gracefully", {
  bad_region <- head(market_share, 2L)
  bad_region$region <- c("a", "b")
  expect_snapshot_error(
    plot_techmixX(bad_region)
  )
})

test_that("with too many scenario_source errors gracefully", {
  bad_scenario_source <- head(market_share, 2L)
  bad_scenario_source$scenario_source <- c("a", "b")
  expect_snapshot_error(
    plot_techmixX(bad_scenario_source)
  )
})

test_that("with too many scenarios errors gracefully", {
  too_many <- head(market_share, 4L)
  too_many$metric <- c("projected", "corporate_economy", "target_a", "target_b")

  expect_error(
    # class = "invalid_length",
    plot_techmixX(too_many),
    "must be.*1.*not 2"
  )
})

test_that("with too few scenarios errors gracefully", {
  too_few <- head(market_share, 2L)
  too_few$metric <- c("projected", "corporate_economy")

  expect_error(
    plot_techmixX(too_few),
    "Can't find.*scenarios"
  )
})

test_that("outputs a ggplot", {
  data <- head(market_share, 3)
  p <- plot_techmixX(data)
  expect_s3_class(p, "ggplot")
})

# prep_techmixB() ----

test_that("prep_techmixB() outputs the expected snapshot", {
  data <- market_share %>%
    filter(
      dplyr::between(year, 2020, 2025),
      scenario_source == "demo_2020",
      sector == "power",
      region == "global",
      metric %in% c("projected", "corporate_economy", "target_sds")
    )

  out <- prep_techmixB(data, value = "technology_share")
  skip("Dead code")
  expect_snapshot(out)
})

test_that("with missing crucial names errors gracefully", {
  data <- head(market_share)

  bad <- select(data, -metric)
  expect_error(class = "missing_names", prep_techmixB(bad))

  bad <- select(data, -technology_share)
  expect_error(class = "missing_names", prep_techmixB(bad, "technology_share"))

  bad <- select(data, -year)
  expect_error(class = "missing_names", prep_techmixB(bad))

  bad <- select(data, -scenario_source)
  expect_error(class = "missing_names", prep_techmixB(bad))
})

test_that("with more than one value of some columns errors gracefully", {
  long_source <- mutate(head(market_share, 2), scenario_source = 1:2)
  skip("Dead code")
  expect_snapshot_error(prep_techmixB(long_source))

  long_sector <- mutate(head(market_share, 2), sector = 1:2)
  skip("Dead code")
  expect_snapshot_error(prep_techmixB(long_sector))

  long_region <- mutate(head(market_share, 2), region = 1:2)
  skip("Dead code")
  expect_snapshot_error(prep_techmixB(long_region))
})

test_that("with bad metric errors gracefully", {
  bad_metric <- mutate(head(market_share), metric = "bad")
  skip("Dead code")
  expect_snapshot_error(prep_techmixB(bad_metric))
})
