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
  has_factors <- any(unlist(lapply(p_data, is.factor)))

  expect_false(has_factors)
})

test_that("doesn't output pretty labels", {
  data <- filter(sda, sector == "automotive")
  p <- plot_emission_intensity(data)

  metrics <- unique(p$data$label)
  ugly <- c("projected", "corporate_economy")
  expect_equal(metrics, ugly)
})

test_that("with too many lines to plot errors gracefully", {
  data <- filter(sda, sector == "cement") %>%
    bind_fake_sda_metrics(8)
  # TODO: Why this warning?
  suppressWarnings(
    expect_snapshot_error(plot_emission_intensity(data))
  )
})
