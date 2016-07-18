#' @name compute_realized_fwer_and_power
#' @aliases compute_realized_fwer_and_power
#' @title compute_realized_fwer_and_power
#' @description compute_realized_fwer_and_power
#' @export
#' 

compute_realized_fwer_and_power <- function(ps,h1=rep(FALSE,length(ps)),alpha=.05){
  if(all(!h1)) 
    power=NA else
      power=mean(ps[h1]<=alpha)
  if(all(h1)) 
    fwer=NA else
      fwer=any(ps[!h1]<=alpha)
  c(fwer=fwer,power=power)
}