test_that("outputs the expected snapshot", {
  data <- sda %>%
    filter(sector == "cement") %>%
    prep_timeline()

  expect_snapshot(timeline_specs(data))
})
