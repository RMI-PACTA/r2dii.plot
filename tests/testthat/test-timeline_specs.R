test_that("outputs the expected snapshot", {
  data <- prepare_for_timeline(sda_target)
  expect_snapshot(timeline_specs(data))
})
