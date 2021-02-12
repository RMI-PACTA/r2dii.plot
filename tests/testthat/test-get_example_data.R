test_that("outputs a data.frame", {
  expect_s3_class(get_example_data(), "data.frame")
})

test_that("has the expected type of columns", {
  expected <- c(
      metric = "character",
      production = "double",
      region = "character",
      scenario_source = "character",
      sector = "character",
      technology = "character",
      technology_share = "double",
      year = "integer"
    )

  data <- get_example_data()
  sorted <- data[sort(names(data))]
  actual <- vapply(sorted, typeof, character(1))

  expect_equal(actual, expected)
})

test_that("outputs the expected snapshot", {
  expect_snapshot(get_example_data())
})

test_that("outputs like r2dii.analysis::target_market_share()", {
  # Such a strong dependency on upstream packages makes this test fragile
  skip_on_cran()

  # This integration (not unit) test checks if this function keeps its promise
  # The call to namespace:: is intentional
  lbk <- r2dii.data::loanbook_demo[1:10, ]
  ald <- r2dii.data::ald_demo[795:800, ]
  matched <- r2dii.match::prioritize(r2dii.match::match_name(lbk, ald))

  scenario <- r2dii.data::scenario_demo_2020
  region <- r2dii.data::region_isos_demo
  banks <- r2dii.analysis::target_market_share(matched, ald, scenario, region)

  sort_df <- function(data) {
    data[sort(names(data))]
  }
  data <- get_example_data()
  actual <- vapply(sort_df(data), typeof, character(1))
  expected <- vapply(sort_df(banks), typeof, character(1))

  expect_equal(actual, expected)
})
