#' @name feat.noinfo
#' @title remove_constant
#' @param dataset
#' @export remove_constant

remove_constant <- function(dataset){
feat.noinfo <- names(which(sapply(dataset,n_distinct,na_rm =TRUE)<=1))
dataset <- dataset[,setdiff(names(dataset),
                            feat.noinfo),with=F]
}