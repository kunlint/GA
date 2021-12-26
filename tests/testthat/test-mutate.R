test_case_1 <- c(0,1,1,1,1,1)
equal_or_not <- function(v1, v2){
  for(i in 1:length(v1)){
    if(v1[i] != v2[i]){
      return(FALSE)
    }
  }
  return(TRUE)
}

test_that("mutate function has equal length when p = 1", {
  expect_true(length(test_case_1) == length(mutate(test_case_1, 1)))
})

test_that("mutate function has equal length when p = 0", {
  expect_true(length(test_case_1) == length(mutate(test_case_1, 0)))
})

test_that("mutate function changes chrome when p = 1", {
  expect_false(equal_or_not(test_case_1, mutate(test_case_1,1)))
})

test_that("mutate function does not change chrome when p = 0", {
  expect_true(equal_or_not(test_case_1, mutate(test_case_1,0)))
})

test_case_2 <- rep(1,100000)
a <- mutate(test_case_2, 1)
table <- table(a)
#test passes since both 0 and 1 are  close to 50000
table[names(table) == 1]
table[names(table) == 0]




