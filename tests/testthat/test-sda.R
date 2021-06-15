test_that("outputs a data.frame", {
  expect_s3_class(sda, "data.frame")
})

test_that("outputs the expected snapshot", {
  skip_if(r_version_is_older_than(4))
  expect_snapshot(sda)
})
