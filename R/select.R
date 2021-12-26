## implement the select main function
#' Select Variables for Regression
#'
#' \code{select} return the best set of predictors for a given response
#'
#' This function uses a simple genetic algorithm to select the best predictors
#' for a given response, using the supplied fitness function, e.g., AIC or BIC
#' scores. Predictors are encoded in a binary fashion, where \code{1} means
#' including the predictor and \code{0} means excluding the predictor. Subjects
#' are grouped into generations, where each subject is a binary vector. In each
#' generation, parents are selected from the subjects according to their fitness
#' rank, and offspring are produced to generate the next generation. In later
#' generations, we can expect predictors (subjects) with good performance
#' persist, and the bad predictors are filtered out, such that we end up with a
#' good (heuristically speaking) set of predictors in the last generation.
#'
#' @param dataset A dataframe containing the response and the predictors.
#' @param response The name of the response in the dataset that we want to predict.
#' @param predictors The names of the predictors. If given, predictors will only
#'     be chosen from the supplied names.
#' @param family Type of regression. Defaults to gaussian.
#' @param max_iter Maximum number of iterations to run. Defaults to \code{100}.
#' @param min_iter Minimum numbers of iterations to run. Defaults to \code{10}.
#' @param fitness_function Function to evaluate the fitted model. Defaults to
#'     \code{AIC}. For user-provided function, it must be able to take an
#'     \code{lm} or \code{glm} object and return a numeric value. The values
#'     should be smaller for better models.
#' @param reltol Relative tolerance. Defaults to \code{1e-8}. If the relative
#'     difference between the fitness scores of the best candidates in the current
#'     generation and the previous generation is smaller than \code{reltol}, and
#'     the genetic diversity is low, we stop and return the result.
#' @param gen_size Number to determine the number of candidates in each
#'     generation. Defaults to 1.5. The generation size is \code{gen_size}
#'     \code{*} \code{num_of_predictors}.
#' @param mutate_prob Probability of mutation. Defaults to \code{0.01}. Each
#'     gene (predictor) mutate independently according to this probability.
#' @param diversity_fact A number in between \code{0} and \code{1} to decide
#'     whether the diversity in the current generation is low. Defaults to
#'     \code{0.1}, i.e., when the number of distinct candidates in the generation
#'     is less than the maximum of \code{0.1} \code{*} \code{generation_size}
#'     and \code{2}, we consider diversity is low.
#' @param details Whether to store the fitness scores along the way. Defaults to
#'     False. If True, also plot the evolving fitness scores for all the generations.
#' @param selection_mode \code{1} or \code{2}. Defaults to \code{1}. If
#'     \code{1}, we choose both parents according to the fitness scores
#'     independently. If \code{2}, one of the parents is chosen according to the
#'     fitness scores, and the other is chosen completely random.
#' @param cross_point Number of cut points to perform cross-over. Defaults to
#'     \code{1}. Should not exceed  \code{num_of_predictors} \code{-} \code{1}.
#' @param generation_gap Ratio of update. Defaults to \code{1}, i.e., update every
#'     subject in the generation when producing offspring. Must be a number
#'     in between \code{0} and \code{1}.
#' @param parallel Whether to use parallel computation, using the R \code{future}
#'     package. Defaults to False. If True, users should designate the number
#'     of cores to use on their end using the \code{plan} function in the
#'     \code{future} package.
#' @param ... other arguments to pass to the R \code{glm}/\code{lm} function.
#' @export
#' @return A named list containing \code{iteration}, the number of iterations,
#'     \code{best_cand}, a character vector containing the names of the best
#'     predictors, \code{fitness_score}, the fitness score of the best predictors
#'     (the negative value of \code{fitness_function} applying to the best
#'     predictors), and \code{fit_scores_mat}, if \code{details} is \code{True},
#'     this is the matrix of the fitness scores for all subjects in all
#'     generations (each column is a generation). In some cases, it is possible
#'     that the best candidate does not exist in the last generation, so
#'     \code{best_so_far} returns the best candidate among all the generations.
#' @examples
#' select(mtcars, "mpg")
#' select(mtcars, "am", family="binomial")
#' select(baseball, "y", generation_gap=0.5)
select <- function(dataset, response, predictors=NULL, family=gaussian(),
                   max_iter=100, min_iter=10, fitness_function=AIC, reltol=1e-8,
                   gen_size=1.5, mutate_prob=0.01,
                   diversity_fact=0.1, details=F, selection_mode=1, generation_gap=1,
                   cross_point=1, parallel=F, ...) {
   # check inputs are valid
   assertthat::assert_that(is.data.frame(dataset), msg="please provide a dataframe!")
   assertthat::assert_that(length(response) == 1, msg="please supply only 1 response!")
   if (!is.null(predictors)) assertthat::assert_that(length(predictors) >= 1,
                                                     msg="please supply at least 1 predictors!")
   assertthat::assert_that(max_iter > min_iter,
                           msg="minimum iteration must be smaller than maximum iteration!")
   assertthat::assert_that(is.numeric(fitness_function(lm(1~1))) &
                             !is.na(fitness_function(lm(1~1))),
                           msg="invalid fitness function!")
   assertthat::assert_that(gen_size > 0, msg="invalid generation size")
   assertthat::assert_that(0 < diversity_fact & diversity_fact < 1,
                           msg="diversity factor must be in between 0 and 1!")
   assertthat::assert_that(selection_mode == 1 | selection_mode == 2,
                           msg="only mode 1 and mode 2 are supported!")
   assertthat::assert_that(generation_gap > 0 & generation_gap <= 1,
                           msg="generation gap must be in between 0 and 1!")

   # default to use all other variables in the dataframe
   if (is.null(predictors)) predictors <- names(dataset)[names(dataset) != response]
   assertthat::assert_that(cross_point < length(predictors),
                           msg="invalid number of cross-over points!")

   # initialize the zeroth generation
   C <- length(predictors)
   P <- ceiling(C * gen_size)
   P <- P + P %% 2  # ensure P is even
   gen0 <- init(P, C)

   cur_gen <- gen0
   cur_gen <- remove_all_zeros(cur_gen) # when C is small it is possible that we have a chromosome with all 0's
   # iteratively update the generation
   if (details) fit_scores_mat <- matrix(NA, nrow=P, ncol=max_iter)
   else fit_scores_mat <- NULL

   fitness_hash <- new.env() # hash table for fitness scores for quick look up
   for (i in seq(1, max_iter)) {
     # select parents
     parent_select <- select_parents(cur_gen, response, dataset, predictors, family=family,
                    fitness_function=fitness_function, details=details,
                    generation_gap=generation_gap, selection_mode=selection_mode,
                    fitness_hash, parallel=parallel, ...)

     parents <- parent_select$parents

     if (details) fit_scores_mat[ ,i] <- parent_select$fitness_scores
     cur_best_subject <- parent_select$best_cand
     cur_fit <- cur_best_subject$fitness_score

     if (i == 1) {
        prev_fit <- cur_fit
        best_fit <- cur_fit
        best_so_far <- predictors[as.logical(cur_best_subject$chromosome)]
     }

     if (i > 1 & cur_fit > best_fit) {
        best_fit <- cur_fit
        best_so_far <- predictors[as.logical(cur_best_subject$chromosome)]
     }

     # after the first `min_iter` iterations
     # check whether the best fitness scores in two consecutive generations are
     # close enough
     # if they are close enough and the number of distinct chromosomes are less
     # than some pre-determined number, break the loop
     if (i > min_iter) {
         if(nrow(dplyr::distinct(data.frame(t(cur_gen)))) < max(2, ceiling(P*diversity_fact)) &
         abs(prev_fit - cur_fit)/(abs(prev_fit) + reltol) < reltol) {
         best_cand <- predictors[as.logical(cur_best_subject$chromosome)]
         if (details) matplot(t(fit_scores_mat), col="black", pch='.',
                              cex=3, ylab="fitness scores", xlab="generations")
         return(list(iteration=i, best_cand=best_cand, fitness_score=cur_fit,
                     fit_scores_mat=fit_scores_mat, best_so_far=best_so_far))
         }
      }

     prev_fit <- cur_fit

     # perform cross-over
     off_springs <- parents

     n_updates <- parent_select$n_updates # how many subjects to update
     parents_to_update <- parents[, 1:(n_updates)]
     dim(parents_to_update) <- c(C * 2, n_updates / 2)
     off_springs[, 1:(n_updates)] <- matrix(unlist(apply(parents_to_update,
                                                         MARGIN=2, FUN=function(parents)
                                          {cross_over(parents[1:C], parents[(C+1):(2*C)],
                                                      cross_point=cross_point)})), nrow=C)


     # perform mutation
     off_springs_mut <- apply(off_springs, MARGIN=2, FUN=mutate, p=mutate_prob)

     # assign result to current generation
     cur_gen <- off_springs_mut
     # if (C < 10)  cur_gen <- remove_all_zeros(cur_gen)
   }

   # final fitness (if we did not converge in the loop)
   if (details) matplot(t(fit_scores_mat), col="black", xlab = "generations",
                        ylab="fitness scores",pch='.', cex=3)
   best_cand <- predictors[as.logical(cur_best_subject$chromosome)]

   return(list(iteration=max_iter, best_cand=best_cand,
               fitness_score=cur_fit, fit_scores_mat=fit_scores_mat,
               best_so_far=best_so_far))
}



