###############
# if x and y are lists with svd elements, it is assumed they have been scaled before svd 

.cc_core <- function(X,Y,numb_cc=NULL){
  ############### CCA
  if(!.is_svd(X)){
    X=scale(X,scale=FALSE)
    svx=svd(X);
  } else svx=X 
  
  if(!.is_svd(X)){
    Y=scale(Y,scale=FALSE)
    svy=svd(Y);
  } else svy=Y

  if(is.null(numb_cc)) numb_cc=min(length(svx$d),length(svy$d))
  
  ###S11^{-1/2} S12 S22^{-1/2} = V1 LDR' V2' = UDV'
  res=svd(t(svx$u)%*%svy$u,nu = numb_cc,nv = numb_cc);
  # res$d
  if(!is.null(res$u)){
    res$u= svx$v%*%res$u
    res$xcoef=res$u
    res$u=NULL}
  if(!is.null(res$v)){
    res$v= svy$v%*%res$v
    res$ycoef=res$v
    res$v=NULL}
  ###
  names(res)[1]="cor"
  return(res)
}


#####################
convert2dummies <- function(Y){
  if(any(apply(Y,2,is.character))){
    all_levs=apply(Y,1,paste,collapse = ":")
    all_levs=factor(all_levs)
    # contrasts(all_levs)<- contr.sum(nlevels(all_levs))
    Y=cbind(model.matrix(~all_levs+0))
    colnames(Y)=gsub("all_levs","",colnames(Y))
  }
  Y
}

fillnas <- function(Y){
  nas=which(is.na(Y),arr.ind = TRUE)
  if(nrow(nas)==0) return(Y)
  Y[nas]=colMeans(Y[,nas[,2],drop=FALSE],na.rm=TRUE)
  Y
}

as_named_matrix <- function(Y,root_name="V"){
  Y=as.matrix(Y)
  if(is.null(colnames(Y)))
    colnames(Y)=paste0(root_name,1:ncol(Y))
  Y
}



.get_explaned_variance_proportion <- function(Y,score){
  expl_var=sapply(1:ncol(score),function(i){
    sc=score[,i,drop=FALSE]
    sc=sc/(t(score[,i,drop=FALSE])%*%score[,i,drop=FALSE])[,]
    proj= sc%*%t(sc)
    res=tr(t(Y)%*%proj%*%Y)
    
  })
  res=expl_var/(tr(t(Y)%*%Y)/(nrow(Y)-1))
  names(res)=colnames(score)
  res
}


######################
.is_svd <- function(X){
  if(!is.list(X)) return(FALSE)
  setequal(x = names(X),y = c("d","u","v"))
}

##############
.compute_stats <- function (res) 
{
  X.aux = scale(res$data$X, center = TRUE, scale = FALSE)
  Y.aux = scale(res$data$Y, center = TRUE, scale = FALSE)
  X.aux[is.na(X.aux)] = 0
  Y.aux[is.na(Y.aux)] = 0
  xscores = X.aux %*% res$xcoef
  yscores = Y.aux %*% res$ycoef
  ### rifare qui: basta X'scores e riscalare
  corr.X.xscores = cor(res$data$X, xscores, use = "pairwise")
  corr.Y.xscores = cor(res$data$Y, xscores, use = "pairwise")
  corr.X.yscores = cor(X, yscores, use = "pairwise")
  corr.Y.yscores = cor(Y, yscores, use = "pairwise")
  
  res$scores=list(xscores = xscores, yscores = yscores, corr.X.xscores = corr.X.xscores, 
              corr.Y.xscores = corr.Y.xscores, corr.X.yscores = corr.X.yscores, 
              corr.Y.yscores = corr.Y.yscores)
  res$prop_expl_var=
    list(X = .get_explaned_variance_proportion(res$data$X,res$scores$xscores),
         Y = .get_explaned_variance_proportion(res$data$Y,res$scores$yscores))
  
  res
}


################
residualize <- function(Y,Z){
  HY=Z%*%solve(t(Z)%*%Z)%*%t(Z)%*%Y
  Y-HY
}

residualizing_matrix <- function(Z,return_Q=TRUE)
{
  res <-  list(IH = diag(nrow(Z)) - Z %*% solve(t(Z)%*%Z) %*% t(Z))
  res$IH <- (res$IH + t(res$IH))/2
  
  if(return_Q){  
    ei = eigen(res$IH)
    if (any(is.complex(ei$values))) {
      warning("Data can not be orthoganalized")
      return(NA)
    }
    ei$vectors <- ei$vectors[, (ei$values > 0.1)]
    res$Q=t(ei$vectors)
  }
  return(res)
}