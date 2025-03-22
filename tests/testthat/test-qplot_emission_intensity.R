test_that("outputs pretty labels", {
  data <- filter(sda_demo, sector == "cement", region == "global")
  p <- qplot_emission_intensity(data)

  metrics <- sort(unique(p$data$label))
  pretty <- sort(
    c("Projected", "Corporate Economy", "Adjusted Scenario Demo", "Target Demo")
  )
  expect_equal(metrics, pretty)
})

test_that("Prints title as expected", {
  data <- filter(sda_demo, sector == "cement", region == "global")
  p <- qplot_emission_intensity(data)

  expect_snapshot_output(p$labels$title)
})

test_that("Prints axis labels as expected", {
  data <- filter(sda_demo, sector == "cement", region == "global")
  p <- qplot_emission_intensity(data)

  expect_equal(p$labels$x, "Year")
  expect_snapshot_output(p$labels$x)

  expect_equal(p$labels$y, bquote("Tons of" ~ CO^2 ~ "per Ton of Production Unit"))
  expect_snapshot_output(p$labels$y)
})

test_that("Plots a data set with maximum time horizon of 5 years", {
  data <- filter(sda_demo, sector == "cement", region == "global")
  p <- qplot_emission_intensity(data)
  expect_true(diff(year_range(p)) <= 5)
})
