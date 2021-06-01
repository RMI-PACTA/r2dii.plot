test_that("outputs a data.frame", {
  expect_s3_class(prep_timeline(sda), "data.frame")
})

test_that("with bad `extrapolate` errors gracefully", {
  expect_error(
    prep_timeline(sda, extrapolate = "bad"),
    "logical.*not TRUE"
  )
})

test_that("outputs column `sector`", {
  out <- prep_timeline(sda)
  expect_true(hasName(out, "sector"))
})

test_that("preserves sectors", {
  out <- prep_timeline(sda)
  expect_equal(unique(out$sector), unique(out$sector))
})

test_that("outputs the expected snapshot", {
  expect_snapshot(prep_timeline(sda))
})

test_that("without crucial columns errors gracefully", {
  expect_error(class = "missing_names", prep_timeline(select(sda, -sector)))
  expect_error(class = "missing_names", prep_timeline(select(sda, -year)))
  expect_error(
    class = "missing_names",
    prep_timeline(select(sda, -emission_factor_metric))
  )
  expect_error(
    class = "missing_names",
    prep_timeline(select(sda, -emission_factor_value))
  )
})

test_that("outputs expected colums", {
  out <- prep_timeline(sda)
  expected <- c("year", "line_name", "value", "extrapolated")
  expect_true(all(expected %in% names(out)))
})
