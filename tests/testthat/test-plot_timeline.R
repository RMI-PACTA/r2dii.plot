test_that("outputs the expected snapshot", {
  skip_if(r_version_is_older_than(4))

  data <- dplyr::tribble(
    ~year,  ~line_name, ~value, ~extrapolated,
     2021,         "a",    0.1,         FALSE,
     2022,         "a",    0.2,         FALSE,
     2023,         "a",    0.3,         FALSE,
     2021,         "b",    0.5,          TRUE,
     2022,         "b",    0.6,          TRUE,
     2023,         "b",    0.7,          TRUE,
  )

  out <- unclass(
    plot_timeline(data)
  )
  out$plot_env <- NULL

  expect_snapshot(out)
})

test_that("with specs missing crucial columns, errors gracefully", {
  data <- fake_timeline_data()
  bad <- timeline_specs(data)
  bad$colour_hex <- NULL

  expect_error(
    class = "missing_names",
    plot_timeline(data, specs = bad)
  )
})

test_that("with too many lines errors gracefully", {
  data <- fake_timeline_data(line_name = letters[1:10])

  expect_error(
    class = "too_many_lines",
    plot_timeline(data)
  )
})

test_that("handles specs with factors", {
  data <- fake_timeline_data(2020:2021)
  specs <- timeline_specs(data)
  specs$label <- as.factor(specs$label)

  expect_no_error(
    plot_timeline(data, specs = specs)
  )
})

test_that("with line_name where specs missmatching data, errors gracefully", {
  data <- fake_timeline_data()
  specs <- timeline_specs(data)
  specs$line_name <- "bad"

  expect_error(
    class = "missmatching_line_name",
    plot_timeline(data, specs)
  )
})

test_that("plots year as 'Date'", {
  data <- fake_timeline_data()

  gg <- plot_timeline(data)
  year <- gg$layers[[1]]$data$year

  expect_s3_class(year, "Date")
})
