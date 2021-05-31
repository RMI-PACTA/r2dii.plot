test_that("outputs the expected snapshot", {
  data <- prep_timelineA(sda, sector_filter = "cement")
  expect_snapshot(timeline_specs(data))
})
