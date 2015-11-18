#' @name fillNA
#' @title fill NAs of a column using all other columns (as covariates in a lm)
#' @param data a datsa.frame
#' @param name.fill the column name to fill the NAs using all other columns
#' @export
#' 
fillNA <- function(data,name.fill){
  mod=lm(paste(name.fill,"~ ."),data=data)
  to.fill=which(is.na(data[,name.fill]))
  data[to.fill,name.fill]=predict(mod,newdata = data[to.fill,])
  data
}