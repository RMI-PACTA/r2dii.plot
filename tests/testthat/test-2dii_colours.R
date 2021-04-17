test_that("outputs the expected date", {
  expect_snapshot(get_r2dii_technology_colours("power"))
  expect_snapshot(r2dii_palette_colours())
  expect_snapshot(r2dii_sector_colours())
})
