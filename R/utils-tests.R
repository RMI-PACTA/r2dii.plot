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

# Like `purrr::map_df()`. Avoid using purrr just for this function.
map_df <- function(.x, .f, ...) {
  .f <- rlang::as_function(.f)
  Reduce(bind_rows, lapply(.x, .f, ...))
}

expect_no_error <- function(...) {
  testthat::expect_error(..., NA)
}

expect_no_message <- function(...) {
  testthat::expect_message(..., NA)
}

# Pull unique values from gg$plot$data
unique_plot_data <- function(p, name) {
  g <- ggplot_build(p)
  unique(g$plot$data[[name]])
}

# Pull unique values from gg$data[[1]]
unique_data1 <- function(p, name) {
  g <- ggplot_build(p)
  unique(g$data[[1]][[name]])
}

year_range <- function(p) {
  range(as.numeric(format(p$data$year, format = "%Y")), na.rm = TRUE)
}

abort_if_year_range_is_5yr_already <- function(data) {
  # This is an assertion inside a test
  # nocov start
  if (diff(range(data$year)) == 5) {
    stop("The default year range must not be 5.", call. = FALSE)
  }
  # nocov end

  invisible(data)
}
