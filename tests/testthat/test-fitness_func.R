response <- "salary"
predictors <- names(baseball)[names(baseball) != response]
test_chromosome <- c(1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0,0,0,0,0,0,0,0,0,0,0,0)
test_chromosome_bad1 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0,0,0,0,0,0,0,0,0,1,0,1)
test_chromosome_bad2 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0,0,0,0,0,0,0,0,0,0,0,0)

#SSE Function to test other valid fitness functions
SSE <- function(model){
  sum(model$residuals^2)
}

#bad SSE Function to test other valid fitness functions
#puts out an error when ran since it doesn't take input model
bad_SSE <- function(model){
  sum(model^2)
}

#another bad SSE Function to test other valid fitness functions
#puts out an error when ran since it doesn't take proper number of inputs
bad_SSE2 <- function(y,y_fitted){
  sum((y-y_fitted)^2)
}

test_that("fitness_func runs on dataset with default parameters", {
  expect_error(fitness_func(data=baseball,response=response,predictors=predictors,
                            subject=test_chromosome), NA)})

test_that("fitness_func runs using BIC", {
expect_error(fitness_func(data=baseball,response=response,predictors=predictors,
                         subject=test_chromosome,fitness_function=BIC,
                         family='gaussian'), NA)})


test_that("fitness_func runs using custom function (SSE)", {
  expect_error(fitness_func(data=baseball,response=response,predictors=predictors,
                            subject=test_chromosome,fitness_function=SSE,
                            family='gaussian'), NA)})

test_that("fitness_func fails using invalid function (bad_SSE)", {
  expect_error(fitness_func(data=baseball,response=response,predictors=predictors,
                            subject=test_chromosome,fitness_function=bad_SSE,
                            family='gaussian'))})

test_that("fitness_func fails using invalid function2 (bad_SSE2)", {
  expect_error(fitness_func(data=baseball,response=response,predictors=predictors,
                            subject=test_chromosome,fitness_function=bad_SSE2,
                            family='gaussian'))})

test_that("fitness_func fails using a subject with more alleles than predictors in dataset", {
  expect_error(fitness_func(data=baseball,response=response,predictors=predictors,
                            subject=test_chromosome_bad1,fitness_function=AIC,
                            family='gaussian'))})

test_that("fitness_func generates higher fitness for worse subject", {
  expect_true(fitness_func(data=baseball,response=response,predictors=predictors,
                           subject=test_chromosome_bad2,fitness_function=AIC,
                           family='gaussian') >
                fitness_func(data=baseball,response=response,predictors=predictors,
                            subject=test_chromosome,fitness_function=AIC,
                            family='gaussian'))})

