#' Transform the correlation into z score
#' @export
rho2z <- function(r)log((1+r) / (1-r)) / 2

#' Compare two correlations
#' @examples 
#' compare_rhos()
#' @export
compare_rhos <- function(r1,r2,n1=61,n2=68){
  z1 <- rho2z(r1)
  z2 <- rho2z(r2)
  z=(z1-z2)/sqrt(1/(n1-3)+1/(n2-3))
  return(list(z=z,p=2*pnorm(-abs(z))))
}

# 
# compare_rhos(.069,.067)
# compare_rhos(-.320,-.166)
# compare_rhos(-.235,-.227)
# 
# compare_rhos(-.319,.068)
# compare_rhos(.185,.247)
# compare_rhos(.044,.125)
# 
# compare_rhos(-.144	,.173)
# compare_rhos(.190,.205)
# compare_rhos(.425,.309)
  

