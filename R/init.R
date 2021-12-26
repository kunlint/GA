# init Function:
#   Function to initiate a matrix with P columns and C rows
#
#   Inputs:
#     P: number of individuals in the starting generation
#     C: length of binary values
#
#   Outputs:
#     v: matrix with P columns and C rows
#
#' Initialize a dataframe with P columns and C rows
#'
#' @param P A number.
#' @param C A number.
#' @return a dataframe with P columns and C rows.
#' @examples
#' init(5, 6)
#' init(3, 4)
#' @export
init <- function(P, C){
  v <- sample(c(1,0), replace=TRUE, size=C)
  i <- 1
  while(i <= P-1){
    ind <- sample(c(1,0), replace=TRUE, size=C)
    v <- cbind(v, ind)
    i <- i + 1
  }
  colnames(v) <- c(rep("ind", P))
  return (v)
}

