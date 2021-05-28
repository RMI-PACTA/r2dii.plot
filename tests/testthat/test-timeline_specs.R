test_that("outputs the expected snapshot", {
  data <- prep__timelineA(sda_target, sector_filter = "cement")
  expect_snapshot(timeline_specs(data))
})
