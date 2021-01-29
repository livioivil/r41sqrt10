#'@description mode
#'@export
#'@title mode
#'@name mode
#'@param v vector of values to compute the mode on.
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#'@description trace of a matrix
#'@export
#'@title tr
#'@name tr
#'@param M matrix.
tr <- function(M) {
  sum(diag(M))
}


#'@description breaks_unit
#'@export
#'@title breaks_unit
#'@name breaks_unit
#'@param x values to use to define the breaks
breaks_unit <- function(x){
  lims=range(x,na.rm=TRUE)+c(-.5,+.5)
  seq(lims[1],lims[2],by=1)
}

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

