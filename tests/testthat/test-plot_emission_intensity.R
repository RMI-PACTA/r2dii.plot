test_that("if `data` is not a data frame errors gracefully", {
  expect_snapshot_error(plot_emission_intensity(1))
})

test_that("if `data` is not sda-like errors gracefully", {
  bad <- head(market_share, 1L)
  expect_snapshot_error(plot_emission_intensity(bad))
})

test_that("if `data` has zero rows errors gracefully", {
  zero_row <- sda[0L, ]
  expect_snapshot_error(plot_emission_intensity(zero_row))
})

test_that("with too many sectors errors gracefully", {
  data <- head(sda, 2)
  data$sector <- c("a", "b")
  expect_snapshot_error(plot_emission_intensity(data))
})

test_that("outputs an object with no factor-columns derived from `specs`", {
  data <- head(filter(sda, sector == "cement"))

  p <- plot_emission_intensity(data)
  p_data <- p$layers[[1]]$data
  specs_cols <- c("emission_factor_metric", "hex")
  has_factors <- any(unlist(lapply(p_data[specs_cols], is.factor)))

  expect_false(has_factors)
})

test_that("outputs pretty labels", {
  data <- filter(sda, sector == "automotive")
  p <- plot_emission_intensity(data)

  get_emission_factor_metric <- function(p) unique(p$layers[[1]]$data$emission_factor_metric)
  expect_equal(get_emission_factor_metric(p), c("Projected", "Corporate Economy"))
})

test_that("with too many lines to plot errors gracefully", {
  add_fake_metrics_sda <- function(data, n) {
    sector <- data$sector[1]
    min_year <- min(data$year)
    max_year <- max(data$year)
    for (i in 1:n) {
      fake_data <- tibble(
        sector = rep(sector, 2),
        year = c(min_year, max_year),
        emission_factor_metric = as.character(i),
        emission_factor_value = NA
      )

      data <- rbind(data, fake_data)
    }
    data
  }

  data <- filter(sda, sector == "cement") %>%
    add_fake_metrics_sda(8)

  expect_snapshot_error(plot_emission_intensity(data))
})

test_that("prep stays the same", {
  data <- tibble::tribble(
     ~sector, ~year,  ~emission_factor_metric, ~emission_factor_value,
    "cement",  2013,              "projected",      0.657950587961228,
    "cement",  2014,              "projected",      0.658859356366402,
    "cement",  2015,              "projected",      0.659768124771575,
    "cement",  2016,              "projected",      0.660676893176748,
    "cement",  2017,              "projected",      0.661585661581922,
    "cement",  2018,              "projected",      0.662494429987095,
    "cement",  2019,              "projected",      0.663403198392268,
    "cement",  2020,              "projected",      0.664311966797442,
    "cement",  2021,              "projected",      0.665220735202615,
    "cement",  2022,              "projected",      0.666129503607788,
    "cement",  2023,              "projected",      0.667038272012962,
    "cement",  2024,              "projected",      0.667947040418135,
    "cement",  2025,              "projected",      0.668855808823308,
    "cement",  2013,      "corporate_economy",      0.661412221772893,
    "cement",  2014,      "corporate_economy",      0.662645159528865,
    "cement",  2015,      "corporate_economy",      0.663842493537321,
    "cement",  2016,      "corporate_economy",      0.665004185461466,
    "cement",  2017,      "corporate_economy",       0.66613019690945,
    "cement",  2018,      "corporate_economy",      0.667220489434263,
    "cement",  2019,      "corporate_economy",      0.668275024533637,
    "cement",  2020,      "corporate_economy",      0.669293763649947,
    "cement",  2021,      "corporate_economy",      0.670276668170116,
    "cement",  2022,      "corporate_economy",      0.671223699425505,
    "cement",  2023,      "corporate_economy",      0.672134818691825,
    "cement",  2024,      "corporate_economy",      0.673009987189028,
    "cement",  2025,      "corporate_economy",       0.67384916608121,
    "cement",  2020,            "target_demo",      0.664311966797442,
    "cement",  2021,            "target_demo",      0.635675754219006,
    "cement",  2022,            "target_demo",       0.60703954164057,
    "cement",  2023,            "target_demo",      0.578403329062134,
    "cement",  2024,            "target_demo",      0.549767116483698,
    "cement",  2025,            "target_demo",      0.521130903905262,
    "cement",  2020, "adjusted_scenario_demo",      0.661412221772893,
    "cement",  2021, "adjusted_scenario_demo",      0.604719745620931,
    "cement",  2022, "adjusted_scenario_demo",      0.548027269468968,
    "cement",  2023, "adjusted_scenario_demo",      0.491334793317006,
    "cement",  2024, "adjusted_scenario_demo",      0.434642317165044,
    "cement",  2025, "adjusted_scenario_demo",      0.377949841013082,
    "cement",  2030, "adjusted_scenario_demo",      0.368501094987755,
    "cement",  2035, "adjusted_scenario_demo",      0.359052348962428,
    "cement",  2040, "adjusted_scenario_demo",      0.236218650633176,
    "cement",  2045, "adjusted_scenario_demo",      0.188974920506541,
    "cement",  2050, "adjusted_scenario_demo",     0.0944874602532704
    )
  expect_snapshot(prep_emission_intensity(data))
})
