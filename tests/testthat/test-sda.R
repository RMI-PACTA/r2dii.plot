test_that("outputs a data.frame", {
  expect_s3_class(sda, "data.frame")
})

test_that("outputs the expected snapshot", {
  skip_if(r_version_is_older_than(4))
  expect_snapshot(sda)
})

test_that("outputs like r2dii.analysis::target_sda()", {
  # This integration (not unit) test checks if this function keeps its promise.
  # It may overlap with other unit tests, but here we focus on dependencies

  # Such a strong dependency on upstream packages makes this test fragile
  skip_on_cran()

  sort_df <- function(data) data[sort(names(data))]

  # Pick small data for speed. "package::f()" is to emphasize integration
  loanbook <- r2dii.data::loanbook_demo[125:150, ]
  ald <- filter(r2dii.data::ald_demo[1:100, ], !is.na(.data$emission_factor))
  scenario <- r2dii.data::co2_intensity_scenario_demo

  expected <- r2dii.match::match_name(loanbook, ald) %>%
    r2dii.match::prioritize() %>%
    r2dii.analysis::target_sda(ald, co2_intensity_scenario = scenario) %>%
    sort_df() %>%
    vapply(typeof, character(1))
  actual <- sda %>%
    sort_df() %>%
    vapply(typeof, character(1))
  expect_equal(actual, expected)
})
