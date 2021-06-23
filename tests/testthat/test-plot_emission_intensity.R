test_that("if `data` is not a data frame errors gracefully", {
  expect_snapshot_error(plot_emission_intensity(1))
})

test_that("if `data` is not sda-like errors gracefully", {
  bad <- head(market_share, 1L)
  expect_snapshot_error(plot_emission_intensity(bad))
})

test_that("if `data` has zero rows errors gracefully", {
  zero_row <- sda[0L, ]
  expect_snapshot_error(plot_emission_intensity(zero_row))
})

test_that("with too many sectors errors gracefully", {
  data <- head(sda, 2)
  data$sector <- c("a", "b")
  expect_snapshot_error(plot_emission_intensity(data))
})

test_that("outputs an object with no factor-columns derived from `specs`", {
  data <- head(filter(sda, sector == "cement"))

  p <- plot_emission_intensity(data)
  p_data <- p$layers[[1]]$data
  has_factors <- any(unlist(lapply(p_data, is.factor)))

  expect_false(has_factors)
})

test_that("with too many lines to plot errors gracefully", {
  data <- filter(sda, sector == "cement") %>%
    bind_fake_sda_metrics(8)
  # TODO: Why this warning?
  suppressWarnings(
    expect_snapshot_error(plot_emission_intensity(data))
  )
})

test_that("outputs colour legend matching lines order", {
  prep <- filter(sda, sector == "cement")

  p <- plot_emission_intensity(prep)
  layers <- p[["layers"]][[1]]
  expr <- rlang::quo_get_expr(layers$mapping$colour)
  # Define `data`. `expr` is `match_lines_order(data)`
  data <- layers$data
  colour <- levels(rlang::eval_tidy(expr))
  legend <- c(
    "Corporate Economy",
    "Projected",
    "DEMO",
    "Adjusted Scenario Demo"
  )

  expect_equal(colour, legend)
})

test_that("if `data` has no `label` we create it", {
  data <- filter(sda, sector == "cement")
  expect_false(has_name(data, "label"))

  p <- plot_emission_intensity(data)
  out <- p[["layers"]][[1]][["data"]]
  expect_true(has_name(out, "label"))
})

test_that("outputs pretty labels", {
  data <- filter(sda, sector == "cement")

  pretty <- c(
    "Adjusted Scenario Demo",
    "Corporate Economy",
    # TODO: Ask if this format is okay
    "DEMO",
    "Projected"
  )
  p <- plot_emission_intensity(data)
  labels <- sort(unique(p[["layers"]][[1]][["data"]][["label"]]))

  expect_equal(labels, pretty)
})

test_that("is sensitive to convert_label", {
  data <- filter(sda, sector == "cement")
  p1 <- plot_emission_intensity(data, convert_label = toupper)
  p2 <- plot_emission_intensity(data, convert_label = tolower)

  label1 <- p1[["layers"]][[1]][["data"]][["label"]]
  label2 <- p2[["layers"]][[1]][["data"]][["label"]]

  expect_false(identical(label1, label2))
  expect_true(identical(tolower(label1), label2))
})
