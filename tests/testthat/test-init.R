test_that("init function produces expected number of rows", {
  expect_equal(nrow(init(3,5)), 5)
})
test_that("init function produces expected number of cols", {
  expect_equal(ncol(init(3,5)), 3)
})
test_that("init function produces expected number of cols", {
  expect_equal(ncol(init(100,5)), 100)
})
test_that("init function with error input", {
  expect_error(init(0,0))
})
test_that("init function with error input", {
  expect_error(init(0,1))
})










