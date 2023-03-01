#' @name rmvnorm_commonrho
#' @export 

rmvnorm_commonrho <- function(n,rho=.5,mu=0,sigma=1){
  eps1=rnorm(mean=0,n=n,sd=sigma)
  eps2=rnorm(mean=0,n=n,sd=sigma)
  eps=rnorm(mean=0,n=n,sd=sigma)
  
  Y <- cbind(y1=(1-rho^2)*eps1+(rho^2)*eps,
             y2=(1-rho^2)*eps2+sign(rho)*(rho^2)*eps)
}