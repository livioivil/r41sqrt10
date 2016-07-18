#' @name t.test.light
#' @title t.test.light
#' @export
#' @aliases t.test.light
#' @title t.test.light
#' @description t.test.light
#' @param Y n X p matrix or data.frame
#' @param tail +1,-1 or 0
#'
t.test.light <- function(Y,tail=1){
  sd=apply(Y,2,sd,na.rm=TRUE)
  n=colSums(!is.na(Y))
  ts= colMeans(Y,na.rm=TRUE)/sd*sqrt(n)
  if(tail==0)  return(list(p=2*pt(-abs(ts),df = n-1),t=ts,df=n-1))
  return(list(p=pt(-sign(tail)*ts,df = n-1),t=ts,df=n-1))
}
