v_1 <- c(1,0,0,1,0,0,1,0,1)
test_case_1 <- matrix(v_1, nrow=3, ncol=3)

v_2 <- c(1,0,0,0,0,0,1,1,1)
test_case_2 <- matrix(v_2, nrow=3, ncol=3)
remove_all_zeros(test_case_2)

v_3 <- c(1,0,1,0,0,0,0,0,0)
test_case_3 <- matrix(v_3, nrow=3, ncol=3)

equal_or_not <- function(v1, v2){
  for(i in 1:length(v1)){
    if(v1[i] != v2[i]){
      return(FALSE)
    }
  }
  return(TRUE)
}

test_that("remove_all_zero works when there is no chrom with all 0", {
  expect_equal(test_case_1, remove_all_zeros(test_case_1))
})

test_that("remove_all_zero removes one all 0 chrome", {
  expect_false(equal_or_not(test_case_2[,2], remove_all_zeros(test_case_1)[,2]))
})

test_that("remove_all_zero removes two all 0 chromes", {
  expect_true(equal_or_not(test_case_3[,2], remove_all_zeros(test_case_3)[,2]) == FALSE
              && equal_or_not(test_case_3[,3], remove_all_zeros(test_case_3)[,3]) == FALSE
  )
})










