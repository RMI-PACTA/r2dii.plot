test_that("abort_if_unknown_values() works as expected", {
  my_data <- tibble(sector = c("power", "automotive"))
  expect_error(
    abort_if_unknown_values("bad", my_data, "sector"), class = "unknown_value"
  )
  expect_error(abort_if_unknown_values("bad", my_data, "sector"), "bad")
  bad <- c("val1", "val2")
  expect_error(abort_if_unknown_values(bad, my_data, "sector"), "val2")
})
