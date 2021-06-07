test_that("outputs the expected snapshot", {
  data <- sda %>%
    filter(sector == "cement") %>%
    prep_timelineY()

  expect_snapshot(timeline_specs(data))
})
