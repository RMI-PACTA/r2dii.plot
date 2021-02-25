test_that("outputs a data.frame", {
  expect_s3_class(sda_target, "data.frame")
})

test_that("has 208 rows and 4 columns", {
  expect_equal(nrow(sda_target), 208)
  expect_equal(ncol(sda_target), 4)
})

test_that(
  "has columns: 'sector', 'year', 'emission_factor_metric', 'emission_factor_value'",
  {
    expect_true(all(colnames(sda_target) %in% c(
      "sector",
      "year",
      "emission_factor_metric",
      "emission_factor_value"
    )))
  }
)

test_that("outputs the expected snapshot", {
  expect_snapshot(sda_target)
})
