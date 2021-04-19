test_that("outputs the expected date", {
  skip_if(r_version_is_older_than(4))

  expect_snapshot(get_r2dii_technology_colours("power"))
  expect_snapshot(r2dii_palette_colours())
  expect_snapshot(r2dii_sector_colours())
})
