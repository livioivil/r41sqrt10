#' @name fillNA
#' @title fill NAs of a column using all other columns (as covariates in a lm)
#' @param data a datsa.frame
#' @param name.fill the column name to fill the NAs using all other columns
#' @param method  ="predict" or a function to be applied to each column of data with at least an NA.
#' @export 

fillNA <- function(data,name.fill=NULL,method="predict"){
  if(is.function(method)){
    for( id in which(apply(data,2,function(x) any(is.na(x))))){
      data[is.na(data[,id]),id]=method(data[,id],na.rm=TRUE)
    } 
    
    } else if(method=="predict"){
    mod=lm(paste(name.fill,"~ ."),data=data)
    to.fill=which(is.na(data[,name.fill]))
    data[to.fill,name.fill]=predict(mod,newdata = data[to.fill,])
  }
  data
}