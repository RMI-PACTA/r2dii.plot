test_that("outputs the expected snapshot", {
  data <- prepare_for_timeline(sda_target, sector_filter = "cement")
  expect_snapshot(timeline_specs(data))
})
