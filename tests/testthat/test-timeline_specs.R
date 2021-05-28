test_that("outputs the expected snapshot", {
  data <- prepare_for_timelineA(sda, sector_filter = "cement")
  expect_snapshot(timeline_specs(data))
})
