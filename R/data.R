#' Baseball Salaries Dataset
#'
#' A dataset containing offensive performance statistics such as batting average,
#' and number of home runs during the 1991 and 1992 seasons.
#' The player's 1992 salaries were also noted and serves as our response variable
#' in the testing of our genetic algorithm. These data are used in the book "Computational Statistics"
#' by G.H. Givens and J.A. Hoeting.
#'
#'
#' @format A data frame with 337 rows and 28 variables:
#' \describe{
#'   \item{salary}{Salary (in thousands of dollars)}
#'   \item{average}{Batting average}
#'   \item{obp}{On-base percentage}
#'   \item{runs}{Number of runs}
#'   \item{hits}{Number of hits}
#'   \item{doubles}{Number of doubles}
#'   \item{triples}{Number of triples}
#'   \item{homeruns}{Number of home runs}
#'   \item{rbis}{Number of runs batted in}
#'   \item{walks}{Number of walks}
#'   \item{sos}{Number of strike-outs}
#'   \item{sbs}{Number of stolen bases}
#'   \item{errors}{Number of errors}
#'   \item{freeagent}{Indicator of "free agency eligibility"}
#'   \item{arbitration}{Indicator of "arbitration eligibility"}
#'   \item{runsperso}{runs/sos}
#'   \item{hitsperso}{hits/sos}
#'   \item{hrsperso}{homeruns/sos}
#'   \item{rbisperso}{rbis/sos}
#'   \item{walksperso}{walks/sos}
#'   \item{obppererror}{obp/errors}
#'   \item{runspererror}{runs/errors}
#'   \item{hitspererror}{hits/errors}
#'   \item{hrspererror}{homeruns/errors}
#'   \item{soserrors}{sos\eqn{*}errors}
#'   \item{sbsobp}{sbs\eqn{*}obp}
#'   \item{sbsruns}{sbs\eqn{*}runs}
#'   \item{sbshits}{sbs\eqn{*}hits}
#' }
#' @source \url{https://www.stat.colostate.edu/computationalstatistics/}
"baseball"

#' Dummy Dataset for Testing
#'
#' A dataset created for the purposes of testing the genetic algorithm correctly
#' identifies the features that contribute to the response variable. This dataset
#' contains 10 predictors, and 1 response variable that's a linear
#' combination of the 10 predictor, with an intercept and noise added.
#' The predictors are all Unif(0,30), the coefficients are norm(0,5), the
#' intercept is norm(100,10), the noise are norm(0,5), and all points are iid
#' generated from the seed 0.
#'
#'
#' @format A data frame with 1000 rows and 11 variables:
#' \describe{
#'   \item{x1}{Uniform Random Variable from 0 - 30}
#'   \item{x2}{Uniform Random Variable from 0 - 30}
#'   \item{x3}{Uniform Random Variable from 0 - 30}
#'   \item{x4}{Uniform Random Variable from 0 - 30}
#'   \item{x5}{Uniform Random Variable from 0 - 30}
#'   \item{x6}{Uniform Random Variable from 0 - 30}
#'   \item{x7}{Uniform Random Variable from 0 - 30}
#'   \item{x8}{Uniform Random Variable from 0 - 30}
#'   \item{x9}{Uniform Random Variable from 0 - 30}
#'   \item{x10}{Uniform Random Variable from 0 - 30}
#'   \item{y}{Linear combination of columns x1 - x5, with incercept and noise added}
#' }
"dummy_data"
