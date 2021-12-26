#dataset, response, predictors=NULL, family=gaussian(),
#max_iter=100, min_iter=10, fitness_function=AIC, reltol=1e-8, gen_size=1.5, mutate_prob=0.01,
#diversity_fact=0.1, details=F, selection_mode=1, cross_point=1

#test different datasets
set.seed(0)
test_that("select runs on baseball dataset with default parameters", {
  expect_equal(select(baseball,'salary')$best_cand,
        c("runs","hits","rbis","sos","sbs","freeagent","arbitration","runsperso","hitsperso"
          ,"hrsperso","rbisperso","walksperso","soserrors","sbsobp"))})

set.seed(0)
test_that("select runs on dummy dataset with default parameters", {
  expect_error(select(dummy_data,'y',max_iter = 25), NA)})

#test runs on diff predictors
test_that("select runs on baseball dataset with runs as response", {
  expect_error(select(baseball,'runs'), NA)})

test_that("select runs on dummy dataset with x1 as response", {
  expect_error(select(dummy_data,'x1',max_iter = 25), NA)})

#test runs on diff families
test_that("select salary on baseball dataset with Gamma as family", {
  expect_error(select(baseball,'average',family=Gamma()), NA)})

test_that("select x1 on dummy dataset with Gamma as family", {
  expect_error(select(dummy_data,'x1',max_iter = 25,family=Gamma()), NA)})

#test max_iters is respected
set.seed(0)
test_that("select does only 20 iterations on baseball dataset when max_iter=20", {
  expect_equal(select(baseball,'salary',max_iter=20)$iteration,20)})

set.seed(0)
test_that("select does only 12 iterations on dummy dataset when max_iter=20", {
  expect_equal(select(dummy_data,'y',max_iter=12)$iteration,12)})

#test min_iter is respected
set.seed(0)
test_that("select does at least 90 iterations on baseball dataset when min_iter=90", {
  expect_true(select(baseball,'salary',min_iter=90)$iteration>=90)})

set.seed(0)
test_that("select does at least 90 iterations on dummy dataset when min_iter=90", {
  expect_true(select(dummy_data,'y',min_iter=90)$iteration>=90)})

#test diff fitness functions
test_that("select runs on baseball dataset with BIC as fitness function", {
  expect_error(select(baseball,'salary',fitness=BIC), NA)})

set.seed(0)
test_that("select gets correct predictors on dummy dataset with BIC as fitness function", {
  expect_equal(select(dummy_data,'y',fitness=BIC)$best_cand,
               c("x1","x2","x3","x4","x5"))})

#test reltol stops sooner when set lower
test_that("select runs on baseball dataset with small reltol", {
  expect_error(select(baseball,'salary',reltol = .Machine$double.xmin), NA)})

test_that("select runs on baseball dataset with large reltol", {
  expect_error(select(baseball,'salary',reltol = 100), NA)})

#test gen_size
set.seed(0)
test_that("select runs on dummy dataset with small gen size, but doesn't get the right predictors", {
  expect_false(all(select(dummy_data,'y',gen_size = 0.1)$best_cand ==
                     c("x1","x2","x3","x4","x5", "x6","x7")))})

set.seed(0)
test_that("select runs on dummy dataset with large gen size, and gets the right predictors", {
  expect_equal(select(dummy_data,'y',gen_size = 10)$best_cand, c("x1","x2","x3","x4","x5"))})

# test mutate_prob
set.seed(0)
test_that("select runs on dummy dataset with no mutations, but doesn't get the right predictors", {
  expect_false(length(select(dummy_data,'y',mutate_prob=0)$best_cand) == 5)})

set.seed(0)
test_that("select runs on dummy dataset with larger mutation, and gets the right predictors", {
  expect_equal(select(dummy_data,'y',mutate_prob=0.03)$best_cand, c("x1","x2","x3","x4","x5"))})

set.seed(0)
test_that("select runs on dummy dataset with mutation probability = 1, but doesn't get the right predictors", {
  expect_false(isTRUE(select(dummy_data,'y',mutate_prob=1)$best_cand == c("x1","x2","x3","x4","x5")))})

set.seed(0)
test_that("select runs on dummy dataset with mutation probability = 1, but it doesn't converge", {
  expect_equal(select(dummy_data,'y',mutate_prob=1)$iteration, 100)})

#test diversity_fact
set.seed(0)
test_that("select runs on dummy dataset with high diversity, and converges", {
  expect_false(select(dummy_data,'y',diversity_fact = 0.5)$iteration == 100)})

set.seed(0)
test_that("select runs on dummy dataset with low diversity, and converges", {
  expect_equal(select(dummy_data,'y',diversity_fact = 0.01)$iteration, 15)})

#test selection_mode
test_that("select runs on dummy dataset with selection_mode=1", {
  expect_error(select(dummy_data,'y',selection_mode = 1), NA)})

test_that("select runs on dummy dataset with selection_mode=2", {
  expect_error(select(dummy_data,'y',selection_mode = 2), NA)})

#test cross_point
set.seed(0)
test_that("select runs on dummy dataset with multiple crossover points, and gets the right predictors", {
  expect_equal(select(dummy_data,'y',cross_point=5)$best_cand, c("x1","x2","x3","x4","x5"))})

#test generation_gap
test_that("select does not run on dummy dataset a generation gap of 0", {
  expect_error(select(dummy_data,'y',generation_gap = 0))})
set.seed(0)
test_that("select runs on dummy dataset with multiple crossover points, and gets the right predictors", {
  expect_equal(select(dummy_data,'y',generation_gap = 0.5)$best_cand, c("x1","x2","x3","x4","x5"))})
