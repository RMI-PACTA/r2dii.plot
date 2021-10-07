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
  p <- ggplot(data = ggplot2::mpg) +
    geom_point(mapping = aes(x = displ, y = hwy, colour = class))
  g <- ggplot_build(p)
  colours_default <- unique(g$data[[1]]["colour"])

  p_after <- p + scale_colour_r2dii_sector()
  g_after <- ggplot_build(p_after)
  colours_changed <- unique(g_after$data[[1]]["colour"])

  expect_false(isTRUE(all.equal(colours_default, colours_changed)))
})

test_that("changes the plot fill as expected", {
  p <- ggplot(data = ggplot2::mpg) +
    geom_histogram(mapping = aes(x = cyl, fill = class), position = "dodge", bins = 5)
  g <- ggplot_build(p)
  colours_default <- unique(g$data[[1]]["fill"])

  p_after <- p + scale_fill_r2dii_sector()
  g_after <- ggplot_build(p_after)
  colours_changed <- unique(g_after$data[[1]]["fill"])

  expect_false(isTRUE(all.equal(colours_default, colours_changed)))
})
