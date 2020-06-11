test_that("check did it split m times", {
  blblm_model <- blblm(Temp ~ Wind, data = airquality, m = 10, B = 100)
  expect_equal(length(blblm_model$estimates), 10)
})
test_that("check did it bootstrap B times", {
  blblm_model <- blblm(Temp ~ Wind, data = airquality, m = 10, B = 100)
  expect_equal(length(blblm_model$estimates[[1]]), 100)
})
test_that("check object return is class blblm or not", {
  blblm_model <- blblm(Temp ~ Wind, data = airquality, m = 10, B = 100)
  expect_s3_class(blblm_model, "blblm")
})
test_that("check parallel", {
  blblm_model <- blblm(Temp ~ Wind, data = airquality, m = 10, B = 100, cluster_size = 2)
  expect_s3_class(blblm_model, "blblm")
})
