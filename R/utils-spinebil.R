#' Generate basis vector in direction i in n dimensions (i <= n)
#'
#' @param i selected direction
#' @param n number of dimensions
#' @return Basis vector
#' @export
basisVector <- function(i, n){
  v <- rep(0,n)
  v[i] <- 1
  v
}

#' Generate 2-d basis in directions i, j in n dimensions (i,j <= n)
#'
#' @param i first basis direction
#' @param j second basis direction
#' @param n number of dimensions
#' @return Basis matrix
#' @export
basisMatrix <- function(i, j, n){
  matrix(c(basisVector(i,n), basisVector(j,n)), ncol=2)
}
