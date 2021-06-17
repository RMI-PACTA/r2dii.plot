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
  specs_cols <- c("line_name", "label", "hex")
  has_factors <- any(unlist(lapply(p_data[specs_cols], is.factor)))

  expect_false(has_factors)
})

test_that("outputs pretty labels", {
  data <- filter(sda, sector == "automotive")
  p <- plot_emission_intensity(data)

  get_line_name <- function(p) unique(p$layers[[1]]$data$line_name)
  expect_equal(get_line_name(p), c("Projected", "Corporate Economy"))
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
