

Height <- c(1,2,3,4,5)
Age <- c(23, 41, 32, 58, 26)
Weight <- c(2213,232,13,24,6)
df <- data.frame(Height, Age,Weight)
response <- "Height"
predictors <- names(df)[names(df) != response]
subject1 <- c(0,1)
subject2 <- c(1,2)

response2 <- "salary"
predictors2 <- names(baseball)[names(baseball) != response2]
test_chromosome <- c(1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0)
test_chromosome_bad1 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
test_chromosome_bad2 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)


test_that("Passes on simple inputs",{
  expect_equal(c("~","a","c"),as.character(construct_formula("a",c("b","c"),c(0,1))))
})

test_that("Passes on datasets with subject1",{
  expect_equal(c("~","Height","Weight"),as.character(construct_formula(response,predictors,subject1)))
})

test_that("Passes on datasets with subject2",{
  expect_equal(c("~","Height","Age + Weight"),as.character(construct_formula(response,predictors,subject2)))
})

test_that("Passes on passed-in dataset",{
  expect_equal(c("~","salary","average + runs + hits + doubles + triples + homeruns + walks + sbs + runsperso + hrsperso + walksperso + obppererror + runspererror + hitspererror + hrspererror + sbsobp + sbshits"),
               as.character(construct_formula(response2, predictors2, subject=test_chromosome)))
})
