test_that("outputs a data.frame", {
  expect_s3_class(sda_demo, "data.frame")
})

test_that("outputs the expected snapshot", {
  skip_if(r_version_is_older_than(4))
  expect_snapshot(sda_demo)
})

test_that("outputs like r2dii.analysis::target_sda()", {
  # This integration (not unit) test checks if this function keeps its promise.
  # It may overlap with other unit tests, but here we focus on dependencies

  # Such a strong dependency on upstream packages makes this test fragile
  skip_on_cran()

  sort_df <- function(data) data[sort(names(data))]

  # Pick small data for speed. "package::f()" is to emphasize integration
  loanbook <- r2dii.data::loanbook_demo
  abcd <- filter(r2dii.data::abcd_demo, !is.na(.data$emission_factor))
  scenario <- r2dii.data::co2_intensity_scenario_demo
  region_isos <- r2dii.data::region_isos_demo

  expected <- r2dii.match::match_name(loanbook, abcd) %>%
    r2dii.match::prioritize() %>%
    r2dii.analysis::target_sda(
      abcd,
      co2_intensity_scenario = scenario,
      region_isos = region_isos
    ) %>%
    sort_df() %>%
    vapply(typeof, character(1))
  actual <- sda_demo %>%
    sort_df() %>%
    vapply(typeof, character(1))
  expect_equal(actual, expected)
})
