test_that("outputs a data.frame", {
  data <- head(sda)
  expect_s3_class(prep_timeline(data), "data.frame")
})

test_that("with bad `extrapolate` errors gracefully", {
  data <- head(sda)
  expect_error(
    prep_timeline(data, extrapolate = "bad"),
    "logical.*not TRUE"
  )
})

test_that("outputs column `sector`", {
  data <- head(sda)
  out <- prep_timeline(data)
  expect_true(hasName(out, "sector"))
})

test_that("preserves sectors", {
  data <- head(sda)
  out <- prep_timeline(data)
  expect_equal(unique(out$sector), unique(out$sector))
})

test_that("outputs the expected snapshot", {
  data <- head(sda)
  expect_snapshot(prep_timeline(data))
})

test_that("without crucial columns errors gracefully", {
  data <- head(sda)

  expect_error(class = "missing_names", prep_timeline(select(data, -sector)))
  expect_error(class = "missing_names", prep_timeline(select(data, -year)))
  expect_error(
    class = "missing_names",
    prep_timeline(select(data, -emission_factor_metric))
  )
  expect_error(
    class = "missing_names",
    prep_timeline(select(data, -emission_factor_value))
  )
})

test_that("outputs expected colums", {
  data <- head(sda)
  out <- prep_timeline(data)
  expected <- c("year", "line_name", "value", "extrapolated")
  expect_true(all(expected %in% names(out)))
})
