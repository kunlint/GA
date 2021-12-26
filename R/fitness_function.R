# Fitness Function:
#   Function to evaluate the fitness score for the Linear Regression model
#   trained on the inputted dataset on the inputted features.
#
#   Inputs:
#     data: Dataframe we are training on
#     y: Dependent variable we are predicting
#     chromosome: Binary vector
#
#   Outputs:
#     fitness_score: AIC score of the linear regression model trained on our data and predictors
#

fitness_func <- function(data, response, predictors, subject,fitness_hash=NULL,
                         fitness_function = AIC,family='gaussian',...){
  if (sum(subject) == 0) return(Inf)
  if (!is.null(fitness_hash[[paste(subject, collapse="")]])) return(fitness_hash[[paste(subject, collapse="")]])
  lm_chromosome <- glm(formula=construct_formula(response, predictors, subject), data=data, family=family,...)
  fitness_score <- fitness_function(lm_chromosome)
  fitness_hash[[paste(subject, collapse="")]] <- fitness_score
  return(fitness_score)
}


# Construct Formula Function:
#   Function to get the formula for the specified features to input into the regression model
#
#   Inputs:
#     data: Dataframe we are training on
#     y: Dependent variable we are predicting
#     chromosome: Binary vector
#
#   Outputs:
#     Formula: Y ~ X1 + X2 + ... for each inputted regressor
#

construct_formula <- function(response, predictor, subject) {
  formula <- formula(paste(c(response, "~",
                             paste(predictor[as.logical(subject)],
                                   collapse="+")), collapse=" "))
  return(formula)
}

