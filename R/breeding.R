# function to produce off-springs from two parents with simple cross-over
cross_over <- function(chrom1, chrom2, cross_point=1) {
	n <- length(chrom1)
	cut_point <- sort(c(1, sample(n-1, size=cross_point), n))
  off_spring1 <- chrom1
  off_spring2 <- chrom2

	for (i in seq(2, cross_point + 2, 2)) {
	  off_spring1[cut_point[i-1]:cut_point[i]] <- chrom2[cut_point[i-1]:cut_point[i]]
	  off_spring2[cut_point[i-1]:cut_point[i]] <- chrom1[cut_point[i-1]:cut_point[i]]
	}
	return(list(off_spring1=off_spring1, off_spring2=off_spring2))
}

# function to perform potential mutation
# assume binary encoding for now
mutate <- function(offspring, p) {
	n <- length(offspring)
	mutate <- runif(n)
	mutate_to_1 <- which(mutate < p/2)
	mutate_to_0 <- which((p/2 < mutate) & (mutate < p))
	offspring[mutate_to_1] <- 1
	offspring[mutate_to_0] <- 0

	return(offspring)

}


# function to select parents
# based on AIC (or other criterion)
# inputs:
#
#	cur_gen : dataframe containing 0 (no), 1 (yes)
#	response : str
#		name of the predicted value
#	dataset : dataframe
#		contains the predictors and the response
#	family : R family object
#		type of regression (e.g., 'gaussian', 'binomial', 'poisson')
# predictors: a vector of strings
#   regressor names
# outputs:
#
#	parents: dataframe containing 0 and 1
#		n/2 pairs of parents, `n` is the number of subjects in each generation
select_parents <- function(cur_gen, response, dataset, predictors, fitness_hash=NULL,
                           family=gaussian(), fitness_function=AIC, details=F,
                           selection_mode=1, generation_gap=1, parallel=F, ...) {
	P <- ncol(cur_gen)
  cur_gen  <- matrix(as.logical(cur_gen), ncol=P)

  if (parallel) {
	  fitness <- future.apply::future_apply(cur_gen, FUN=function(subject) {
	    -fitness_func(data=dataset, response=response, predictors=predictors,
	                  subject=subject, fitness_function=fitness_function,
	                  family=family, fitness_hash=fitness_hash, ...)}, MARGIN=2)
  }
  else {
    fitness <- apply(cur_gen, FUN=function(subject) {
      -fitness_func(data=dataset, response=response, predictors=predictors,
                    subject=subject, fitness_function=fitness_function,
                    family=family, fitness_hash=fitness_hash, ...)}, MARGIN=2)
  }

  best_subject_index <- which.max(fitness)
  best_cand <- list(chromosome=cur_gen[, best_subject_index],
                    fitness_score=fitness[best_subject_index])

	prob <- 2 * rank(fitness)/(ncol(cur_gen) * (ncol(cur_gen)+1))

	select_index <- rep(NA, P)

	n_updates <- P # number of subjects to update (defaults to update all)
	# if gen_gap < 1, perform partial update
	if (generation_gap < 1) {
	  n_updates <- ceiling(generation_gap * P)
	  n_updates <- n_updates + n_updates %% 2
	  # copy the top (P- n_updates) subjects
	  select_index[(n_updates+1): P] <- sort(fitness, decreasing=T,
	                                         index.return=T)$ix[1:(P - n_updates)]
	}
  # mode 1 : two parents, each chosen independently according to the rank
	if (selection_mode == 1)  select_index[1 : n_updates] <- sample(P, prob=prob,
	                                                                replace=T,
                                                                  size=n_updates)
	# mode 2 : two parents, one chosen according to the rank, and the other
	# completely random
	if (selection_mode == 2)  {
	  select_index[seq(1, n_updates, 2)] <- sample(P, prob=prob, replace=T,
	                                               size=n_updates/2)
	  select_index[seq(2, n_updates, 2)] <- sample(P, replace=T,
	                                               size=n_updates/2)
	}

	fitness_scores <- NULL
	if (details) fitness_scores <- fitness

	return(list(parents=matrix(as.numeric(cur_gen[, select_index]), ncol=P),
	            best_cand=best_cand, fitness_scores=fitness_scores, n_updates=n_updates))
}

# ensure none of the chromosomes in the current generation has all 0's
remove_all_zeros <- function(cur_gen) {
  zero_subject_index <- which(apply(cur_gen, FUN=sum, MARGIN=2) == 0)
  C <- nrow(cur_gen)
  replace_index <- sample(C, size=1)
  cur_gen[replace_index, zero_subject_index]  <- 1
  return(cur_gen)
}
