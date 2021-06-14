test_that("outputs the expected ggplot object", {
  mauro <- path.expand("~") == "/home/mauro"
  skip_if_not(mauro, message = "Brittle test meant to run on mauro's pc only")

  data <- market_share %>%
    filter(
      sector == first(sector),
      technology == first(technology)
    )
  p <- plot_trajectory(data)
  p$plot_env <- NULL
  expect_snapshot(str(p))
})

test_that("without a data frame errors gracefully", {
  expect_snapshot_error(plot_trajectory(1))
})

test_that("without market_share-like data errors gracefully", {
  bad <- sda
  expect_snapshot_error(plot_trajectory(bad))
})

test_that("with cero-row data errors gracefully", {
  cero_row <- market_share[0L, ]
  expect_snapshot_error(
    plot_trajectory(cero_row)
  )
})

test_that("outputs a ggplot", {
  data <- market_share %>%
    filter(
      sector == "power",
      technology == "renewablescap",
      region == "global",
      scenario_source == "demo_2020"
    )

  p <- plot_trajectory(data, normalize = TRUE)
  expect_s3_class(p, "ggplot")
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
  expect_snapshot_error(
    plot_trajectory(bad_region)
  )
})

test_that("with too many scenario_source errors gracefully", {
  bad_scenario_source <- head(market_share, 2L)
  bad_scenario_source$scenario_source <- c("a", "b")
  expect_snapshot_error(
    plot_trajectory(bad_scenario_source)
  )
})

test_that("with inexistent `main_line` errors gracefully", {
  data <- head(market_share, 1L)
  expect_snapshot_error(plot_trajectory(data, main_line = "bad"))
})

test_that("with too many scenarios errors gracefully", {
  data <- head(market_share, 7)
  data$metric <- c(
    "projected",
    "corporate_economy",
    glue("target_{letters[1:5]}")
  )
  expect_snapshot_error(plot_trajectory(data))
})

test_that("is sensitive to `main_line`", {
  data <- market_share %>%
    filter(
      sector == first(sector),
      year >= 2025,
      technology == first(technology)
    )
  expect_no_error(plot_trajectory(data, main_line = "corporate_economy"))
})

test_that("with too long `main_line` errors gracefully", {
  data <- head(market_share, 1L)
  expect_snapshot_error(
    plot_trajectory(data, main_line = c("too", "long"))
  )
})

# prep_trajectoryB() ----

test_that("prep_trajectoryB() outputs the expected snapshot", {
  data <- market_share %>%
    filter(
      technology == "oilcap",
      region == "global",
      scenario_source == "demo_2020",
      year <= 2025,
      sector == "power"
    )
  out <- prep_trajectoryB(data)

  skip("Dead code")
  expect_snapshot(out)
})

test_that("with multiple distinct values in some columns errors gracefully", {
  long_sector <- mutate(head(market_share, 2), sector = 1:2)
  skip("Dead code")
  expect_snapshot_error(prep_trajectoryB(long_sector))

  long_tech <- mutate(head(market_share, 2), technology = 1:2)
  skip("Dead code")
  expect_snapshot_error(prep_trajectoryB(long_tech))

  long_region <- mutate(head(market_share, 2), region = 1:2)
  skip("Dead code")
  expect_snapshot_error(prep_trajectoryB(long_region))

  long_source <- mutate(head(market_share, 2), scenario_source = 1:2)
  skip("Dead code")
  expect_snapshot_error(prep_trajectoryB(long_source))
})

test_that("if `normalize` isn't length-1 errors gracefully", {
  skip("Dead code")
  expect_snapshot_error(
    prep_trajectoryB(market_share, normalize = c(TRUE, TRUE))
  )
})

test_that("if `normalize` isn't logical errors gracefully", {
  skip("Dead code")
  expect_snapshot_error(
    prep_trajectoryB(market_share, normalize = "a")
  )
})

test_that("is sensitive to `normalize`", {
  data <- market_share %>%
    filter(
      technology == "oilcap",
      region == "global",
      scenario_source == "demo_2020",
      sector == "power"
    )

  expect_false(
    identical(
      prep_trajectoryB(data, normalize = TRUE),
      prep_trajectoryB(data, normalize = FALSE)
    )
  )
})

test_that("with missing crucial names errors gracefully", {
  data <- head(market_share)

  bad <- select(data, -metric)
  expect_error(class = "missing_names", prep_trajectoryB(bad))

  bad <- select(data, -sector)
  expect_error(class = "missing_names", prep_trajectoryB(bad))

  bad <- select(data, -technology)
  expect_error(class = "missing_names", prep_trajectoryB(bad))

  bad <- select(data, -region)
  expect_error(class = "missing_names", prep_trajectoryB(bad))

  bad <- select(data, -year)
  expect_error(class = "missing_names", prep_trajectoryB(bad))

  bad <- select(data, -scenario_source)
  expect_error(class = "missing_names", prep_trajectoryB(bad))

  bad <- select(data, -production)
  expect_error(class = "missing_names", prep_trajectoryB(bad))
})

test_that("integrates with plot_trajectory()", {
  data <- market_share %>%
    filter(
      technology == "oilcap",
      region == "global",
      scenario_source == "demo_2020",
      year <= 2025,
      sector == "power"
    )

  out <- prep_trajectoryB(data)
  expect_no_error(plot_trajectoryB(out))
})
