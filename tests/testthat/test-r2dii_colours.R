test_that("outputs the expected snapshot", {
  expect_snapshot({
    palette_colours

    scenario_colours

    sector_colours

    technology_colours
  })
})

test_that("scenario_colours has five rows", {
  # Requirement of get_ordered_scenario_colours()
  expect_equal(nrow(scenario_colours), 5L)
})
