#' rmvnorm_commonrho
#' @name rmvnorm_commonrho
#' @export 

rmvnorm_commonrho <- function(n=10,m=2,rho=.5,mu=0,sigma=1){
  eps=matrix(rnorm(mean=mu,n=n*m,sd=sigma*sqrt(1-rho)),n,m)
  lat=matrix(rnorm(mean=mu,n=n,sd=sigma*sqrt(rho)),n,m)
  eps+lat
}
