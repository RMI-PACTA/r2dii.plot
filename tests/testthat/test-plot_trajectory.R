test_that("isn't restricted to plotting only 5 years", {
  data <- example_market_share()
  expect_true(diff(range(data$year)) > 5L)

  p <- plot_trajectory(data)
  expect_true(diff(range(p$data$year)) > 5L)
})

test_that("outputs verbatim labels", {
  data <- example_market_share()

  verbatim <- sort(unique(data$metric))
  p <- plot_trajectory(data)
  labels <- sort(unique(p$layers[[3]]$data$label))

  expect_equal(labels, verbatim)
})

test_that("outputs no title", {
  data <- example_market_share()
  p <- plot_trajectory(data)

  expect_false("title" %in% names(p$labels))
})

test_that("outputs no subtitle", {
  data <- example_market_share()
  p <- plot_trajectory(data)

  expect_false("subtitle" %in% names(p$labels))
})

test_that("outputs default axis labels", {
  data <- example_market_share()
  p <- plot_trajectory(data)

  expect_equal(p$labels$x, "year")
  expect_equal(p$labels$y, "value")
})

test_that("the errors message includes the name of the user's data", {
  # Keep even if already tested in qplot_. Non-standard evaluation is fragile
  bad_region <- head(market_share, 2L)
  bad_region$region <- c("a", "b")
  expect_error(plot_trajectory(bad_region), "bad_region")
})
