test_that("outputs a data.frame", {
  expect_s3_class(get_example_data(), "data.frame")
})

test_that("has the expected type of columns", {
  expected <- c(
      metric = "character",
      production = "double",
      region = "character",
      scenario_source = "character",
      sector = "character",
      technology = "character",
      technology_share = "double",
      year = "double"
    )

  data <- get_example_data()
  sorted <- data[sort(names(data))]
  actual <- vapply(sorted, typeof, character(1))

  expect_equal(actual, expected)
})
