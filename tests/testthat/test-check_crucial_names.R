test_that("with unexpected names throws a gracefull error", {
  expect_snapshot_error(abort_if_missing_names(tibble(x = 1), c("bad1", "bad2")))
})

test_that("with expected names returns first argument invisibly", {
  data <- tibble(x = 1)
  expect_invisible(out <- abort_if_missing_names(data, "x"))
  expect_identical(data, out)
})

test_that("with expected names is silent", {
  expect_silent(abort_if_missing_names(c(a = 1), "a"))
})
