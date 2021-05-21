test_that("get_ordered_scenario_colours errors with `n` outside valid range", {
  expect_snapshot_error(get_ordered_scenario_colours(1))
  expect_snapshot_error(get_ordered_scenario_colours(6))
})

test_that("get_ordered_scenario_colours works with `n` in valid range", {
  expect_equal(nrow(get_ordered_scenario_colours(2)), 2)
  expect_equal(nrow(get_ordered_scenario_colours(3)), 3)
  expect_equal(nrow(get_ordered_scenario_colours(4)), 4)
  expect_equal(nrow(get_ordered_scenario_colours(5)), 5)
})
