test_that("outputs a data.frame", {
  expect_s3_class(market_share, "data.frame")
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

  data <- market_share
  sorted <- data[sort(names(data))]
  actual <- vapply(sorted, typeof, character(1))

  expect_equal(actual, expected)
})

test_that("outputs the expected snapshot", {
  skip_if(r_version_is_older_than(4))

  some_market_share <- head(as.data.frame(market_share))
  expect_snapshot(some_market_share)
})

test_that("outputs like r2dii.analysis::target_market_share()", {
  # This integration (not unit) test checks if this function keeps its promise.
  # It may overlap with other unit tests, but here we focus on dependencies

  # Such a strong dependency on upstream packages makes this test fragile
  skip_on_cran()

  sort_df <- function(data) data[sort(names(data))]

  # The style `namespace::fun()` highlights this is an integration test
  lbk <- r2dii.data::loanbook_demo[1:10, ]
  ald <- r2dii.data::ald_demo[795:800, ]
  matched <- r2dii.match::prioritize(r2dii.match::match_name(lbk, ald))

  scenario <- r2dii.data::scenario_demo_2020
  region <- r2dii.data::region_isos_demo
  exp_df <- r2dii.analysis::target_market_share(matched, ald, scenario, region)
  expected <- vapply(sort_df(exp_df), typeof, character(1))

  act_df <- market_share
  actual <- vapply(sort_df(act_df), typeof, character(1))

  expect_equal(actual, expected)
})
