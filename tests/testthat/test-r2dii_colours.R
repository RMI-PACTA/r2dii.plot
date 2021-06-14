test_that("outputs the expected snapshot", {
  expect_snapshot({
    palette_colours

    scenario_colours

    sector_colours

    technology_colours

  })
})
