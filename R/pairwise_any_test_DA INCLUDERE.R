# groups=dati$names_groups
#' pairwise.any.test with post hoc correction. Multivariate output is allowed. The results are stored in the third dimension of the array.
#' @param test_fun a function that has data as input and a p-value as output. it is computed for each pair
#' @param groups a vector of group ids
#' @param data a data.frame or matrix to be use by `test_fun`  
#' @param p.adjust.methods any method used by p.adjust + "Shaffer" + "closed" testing (in this case `test_fun` is used in every element of the closure set. NOT implemented, yet!)
#' 

pairwise.any.test <- function(test_fun,groups,data,p.adjust.methods="Shaffer"){
  
  groups=factor(groups)
names_groups=levels(groups)
n_groups=nlevels(groups)
groups=as.numeric(groups)

comp2groups=matrix(NA,n_groups,n_groups)
colnames(comp2groups) <- rownames(comp2groups) <-names_groups

if(n_groups>2){
  for (c1 in 1:(n_groups-1))
    for (c2 in (c1+1):n_groups){
      dt=subset(x = data,subset = groups%in%c(c1,c2))
      comp2groups[c1,c2]=    test_fun(data=dt)
    }
# print(comp2groups)
# browser()
  if(p.adjust.methods=="Shaffer"){
    out=adjustShaffer(abs(comp2groups)) 
    if(any(comp2groups[!is.na(comp2groups)]<0)){
      id_put=which(!is.na(comp2groups),arr.ind = TRUE)
      out[id_put]=comp2groups[!is.na(comp2groups)]
    }
  } else {
    id_put=which(!is.na(comp2groups),arr.ind = TRUE)
    id_put=id_put[,2:1]
    out=comp2groups
    out[id_put]=p.adjust(abs(comp2groups[!is.na(comp2groups)]),method = p.adjust.methods)
    names(dimnames(out))=c("Adjsuted","Raw")
  }
} else{
  tmp=test_fun(data=data)
  comp2groups[1,2]<- comp2groups[2,1] <- tmp
  out <- comp2groups
}
  
  
out
}