test_that("outputs a data.frame", {
  expect_s3_class(sda_target, "data.frame")
})

test_that("outputs the expected snapshot", {
  expect_snapshot(sda_target)
})
