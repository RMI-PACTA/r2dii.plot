test_that("get_ordered_scenario_colours errors with `n` outside valid range", {
  expect_snapshot_error(get_ordered_scenario_colours(1))
  expect_snapshot_error(get_ordered_scenario_colours(6))
})
