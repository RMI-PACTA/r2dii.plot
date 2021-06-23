bind_fake_market_share_metrics <- function(data, n, prefix = "") {
  metrics <- glue("{prefix}{letters[seq_len(n)]}")
  fake <- metrics %>%
    map_df(~ fake_market_share(
      data,
      year = range(data$year),
      metric = .x
    ))

  bind_rows(data, fake)
}

bind_fake_sda_metrics <- function(data, n) {
  metrics <- as.character(seq_len(n))
  fake <- metrics %>%
    map_df(~ fake_sda(
      data,
      year = range(data$year),
      emission_factor_metric = .x
    ))

  bind_rows(data, fake)
}

# FIXME: doesn't work if metric is not passed - rename or recode
fake_market_share <- function(data, ...) {
  tibble(
    sector = first(data$sector),
    technology = first(data$technology),
    region = first(data$region),
    scenario_source = first(data$scenario_source),
    production = 100,
    technology_share = 0.1,
    ...
  )
}

fake_sda <- function(data, ...) {
  tibble(
    sector = first(data$sector),
    emission_factor_value = NA_real_,
    ...
  )
}

map_df <- function(.x, .f, ...) {
  .f <- rlang::as_function(.f)
  Reduce(rbind, lapply(.x, .f, ...))
}

expect_no_error <- function(...) {
  testthat::expect_error(..., NA)
}

expect_no_message <- function(...) {
  testthat::expect_message(..., NA)
}
