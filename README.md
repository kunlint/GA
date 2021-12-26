# Genetic Algorithm For Variable Selection with Regression

## Introduction
This package implements the Genetic Algoirthm for variable(predictor) selection based on *Givens, G. H., &amp; Hoeting, J. A. (2013). Combinatorial Optimization. In Computational statistics. essay, Wiley*. The goal is to find a heurstically good set of predictors based on some pre-determined criteria (e.g., AIC/BIC scores), when the total number of predictors that we are regressing on is large and an exhasutive search is time-consuming or intractable. 

Each possible set of predictors is represented as a binary vector, where 0 means exclusion of the predictor and 1 means inclusion. Sets of predictors are grouped together into a generation. In each iteration of the algorithm, each binary vector is selected according to its fitness scores (e.g., AIC/BIC scores) to be the parent to produce offspring for the next generation. Higher fitness means higher probability of being selected as parents; this imitates the rule of Darwinian evolution. After parent selection, offspring are produced with cross-over; each offspring inherits parts of the binary vectors from its 2 parents. Immediately after cross-over, mutation changes the binary vectors of the offspring by flipping the encoding from 0 to 1 or from 1 to 0 with a small probability. This consitutes the production of a new generation.

## Install
```
R CMD INSTALL GA.tar.gz
```

## Example
```
select(baseball, "salary", details=T)
```

The following plot shows the evolving fitness scores (in this case, the negative AIC scores) for the baseball dataset (Givens et al., 2013). The fitness scores slowly increases as the generations evolve.  
![Alt text](baseball.png?raw=true "Evolving Fitness Scores using the Baseball dataset")
