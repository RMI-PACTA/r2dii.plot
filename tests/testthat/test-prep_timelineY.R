test_that("outputs a data.frame", {
  data <- head(sda)
  expect_s3_class(prep_timelineY(data), "data.frame")
})

test_that("with bad `extrapolate` errors gracefully", {
  data <- head(sda)
  expect_error(
    prep_timelineY(data, extrapolate = "bad"),
    "logical.*not TRUE"
  )
})

test_that("with bad `line` errors gracefully", {
  data <- head(sda)
  expect_error(
    prep_timelineY(data, metric = 1L),
    "character.*not TRUE"
  )
})

test_that("with bad `value` errors gracefully", {
  data <- head(sda)
  expect_error(
    prep_timelineY(data, value = 1L),
    "character.*not TRUE"
  )
})

test_that("outputs column `sector`", {
  data <- head(sda)
  out <- prep_timelineY(data)
  expect_true(hasName(out, "sector"))
})

test_that("preserves sectors", {
  data <- head(sda)
  out <- prep_timelineY(data)
  expect_equal(unique(out$sector), unique(out$sector))
})

test_that("outputs the expected snapshot", {
  data <- head(sda)
  expect_snapshot(prep_timelineY(data))
})

test_that("without crucial columns errors gracefully", {
  data <- head(sda)

  expect_error(class = "missing_names", prep_timelineY(select(data, -sector)))
  expect_error(class = "missing_names", prep_timelineY(select(data, -year)))
  expect_error(
    class = "missing_names",
    prep_timelineY(select(data, -emission_factor_metric))
  )
  expect_error(
    class = "missing_names",
    prep_timelineY(select(data, -emission_factor_value))
  )
})

test_that("outputs expected colums", {
  data <- head(sda)
  out <- prep_timelineY(data)
  expected <- c("year", "line_name", "value", "extrapolated")
  expect_true(all(expected %in% names(out)))
})

test_that("is sensitive to `value`", {
  data <-  rename(head(sda, 1), custom = "emission_factor_value")
  expect_no_error(
    prep_timelineY(data, value = "custom")
  )
})

test_that("is sensitive to `line`", {
  data <- dplyr::rename(head(sda, 1L), custom = "emission_factor_metric")
  expect_no_error(
    prep_timelineY(data, metric = "custom")
  )
})

test_that("is sensitive to `sector_filter`", {
  data <- head(sda, 2)
  data$sector <- c("a", "b")

  default <- NULL
  expect_no_error(prep_timelineY(data, sector_filter = default))

  out <- prep_timelineY(data, sector_filter = "a")
  expect_equal(unique(out$sector), "a")

  too_long <- c("a", "b")
  expect_snapshot_error(
    prep_timelineY(data, sector_filter = too_long)
  )
})

test_that("with bad `sector_filter` errors gracefully", {
  data <- head(sda, 2)
  too_long <- c("a", "b")
  expect_snapshot_error(
    prep_timelineY(data, sector_filter = too_long)
  )

  expect_snapshot_error(
    prep_timelineY(data, sector_filter = TRUE)
  )

})
