#' @name rmat
#' @export rmat
#' @aliases rmat
#' @title random matrix
rmat <- function(n=10,p=3) matrix(rnorm(n*p),n,p)

#' @name projmat
#' @export projmat
#' @aliases projmat
#' @title contruct a projection matrix
projmat <- function(Z) Z%*%solve(t(Z)%*%Z)%*%t(Z)
