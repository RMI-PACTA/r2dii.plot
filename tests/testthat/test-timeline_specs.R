test_that("outputs the expected snapshot", {
  data <- prepare_for_timeline(sda_target)
  expect_snapshot(timeline_specs(data))
})

test_that("works with year of class 'Date'", {
  data <- sda_target %>%
    filter(dplyr::between(year, 2020, 2030)) %>%
    prepare_for_timeline(sector_filter = "cement")

  expect_no_error(timeline_specs(data))
})
