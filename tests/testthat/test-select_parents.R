response <- "salary"
dummy_predictors <- names(dummy_data)[names(dummy_data) != "y"]
predictors <- names(baseball)[names(baseball) != response]

#dataset, response, predictors=NULL, family=gaussian(),
#selection_mode=1, details=F, fitness_function=AIC

#test with wrong input
set.seed(1)
test_that("on baseball dataset select_parents have wrong input", {
  expect_error(select_parents(init(1,15), response, baseball, predictors)$parents)
})

test_that("on baseball dataset select_parents have wrong input", {
  expect_error(select_parents(init(1,15), response, dummy_data, dummy_predictors)$parents)
})

#test under Gaussian
set.seed(1)
test_that("on baseball dataset select_parents have same number of rows under Gaussian", {
  expect_equal(nrow(select_parents(init(10,27), response, baseball, predictors)$parents),27)
})

test_that("on baseball dataset select_parents have same number of cols under Gaussian", {
  expect_equal(ncol(select_parents(init(10,16), response, baseball, predictors)$parents),10)
})

test_that("on dummydata select_parents have same number of rows under Gaussian", {
  expect_equal(nrow(select_parents(init(9,10), "y", dummy_data, dummy_predictors)$parents),10)
})

test_that("on dummydata select_parents have same number of cols under Gaussian", {
  expect_equal(ncol(select_parents(init(9,10), "y", dummy_data, dummy_predictors)$parents),9)
})

#test with different response
set.seed(1)
test_that("on baseball dataset select_parents runs with x1 as response", {
  expect_warning(select_parents(init(10,27), response="average", baseball, predictors))
})

test_that("on dummydata select_parents runs with x1 as response", {
  expect_warning(select_parents(init(9,10), response="x1", dummy_data, dummy_predictors))
})

#test with different selection_mode
set.seed(1)
test_that("on baseball dataset select_parents have same number of rows under diff selection_mode", {
  expect_equal(nrow(select_parents(init(20,27), response, baseball, predictors, selection_mode=2)$parents),27)
})

test_that("on baseball dataset select_parents have same number of rows under diff selection_mode", {
  expect_equal(ncol(select_parents(init(10,16), response, baseball, predictors, selection_mode=2)$parents),10)
})

test_that("on dummydata select_parents have same number of rows under diff selection_mode", {
  expect_equal(nrow(select_parents(init(8,10), "y", dummy_data, dummy_predictors, selection_mode=2)$parents),10)
})

test_that("on dummydata select_parents have same number of rows under diff selection_mode", {
  expect_equal(ncol(select_parents(init(8,10), "y", dummy_data, dummy_predictors, selection_mode=2)$parents),8)
})





