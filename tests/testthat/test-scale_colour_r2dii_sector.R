test_that("outputs a gg ScaleDiscrete", {
  expect_s3_class(scale_colour_r2dii_sector(), c("gg"))
  expect_s3_class(scale_colour_r2dii_sector(), c("ScaleDiscrete"))
  expect_s3_class(scale_fill_r2dii_sector(), c("gg"))
  expect_s3_class(scale_fill_r2dii_sector(), c("ScaleDiscrete"))
})

test_that("if with bad `sectors` errors gracefully", {
  expect_snapshot_error(scale_colour_r2dii_sector(sectors = c("bad")))
  expect_snapshot_error(scale_fill_r2dii_sector(sectors = c("bad")))
})

test_that("changes the plot colours as expected", {
  p <- ggplot(ggplot2::mpg) +
    geom_point(aes(x = displ, y = hwy, colour = class))

  colours_default <- unique_data1(p, "colour")
  colours_changed <- unique_data1(p + scale_colour_r2dii_sector(), "colour")

  expect_false(identical(colours_default, colours_changed))
})

test_that("changes the plot fill as expected", {
  p <- ggplot(ggplot2::mpg) +
    geom_histogram(aes(x = cyl, fill = class), position = "dodge", bins = 5)

  colours_default <- unique_data1(p, "fill")
  colours_changed <- unique_data1(p + scale_fill_r2dii_sector(), "fill")

  expect_false(identical(colours_default, colours_changed))
})
