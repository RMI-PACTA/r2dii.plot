test_that("outputs a gg theme", {
  expect_s3_class(theme_2dii(), c("gg"))
  expect_s3_class(theme_2dii(), c("theme"))
})
