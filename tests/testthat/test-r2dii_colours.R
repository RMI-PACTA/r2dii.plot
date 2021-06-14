test_that("outputs the expected snapshot", {
  expect_snapshot({
    palette_colours

    scenario_colours

    sector_colours

    technology_colours
  })
})

test_that("technology_colours has `label`", {
  expect_true(rlang::has_name(technology_colours, "label"))
})
