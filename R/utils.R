#' come by ma restituisce un data.frame
#' @export
#' @param ... as in by()
by.df<-function(...){
  rr=by(...)
  attr(rr,"class")="list"
  rr=data.frame(rr)
  rr=t(rr)
  if(!is.null(list(...)$INDICES)){
    rownames(rr)=unique(list(...)$INDICES)
  } else rownames(rr)=unique(list(...)[[2]])
  rr
}

##############
colVars <- function(X,...) apply(X,2,var,...)
rowVars <- function(X,...) apply(X,1,var,...)

