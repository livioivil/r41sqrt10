#'cc
#'@description Very similar \code{cc()} of package \code{CCA}, but 1) it allows for X and Y to be rank deficient, 2) it allows for categorical variables and 3) more (see below).
#'@param X See \code{\link[CCA]{cc}} for a proper documentation.
#'@param Y See \code{\link[CCA]{cc}} for a proper documentation.
#'@param Zx (\code{=NULL} by default) covariates of \code{X}. If different from \code{NULL}, the \code{X} are residualized by \code{Zx} before enter \code{cc()}.
#'@param Zy (\code{=Zx} by default) covariates of \code{Y}. Same use of \code{Zx}.
#'@param fill.na replace \code{NA} in \code{X} and \code{Y} with column mean before enter \code{cc()}.
#'@export

cc<-function (X,Y,Zx=NULL,Zy=Zx,fill.na=FALSE) 
{
  Zy
  Y=convert2dummies(Y)
  Y=fillnas(Y)
  X=convert2dummies(X)
  X=fillnas(X)
  
  if(!is.null(Zx))   {
    Zx=convert2dummies(Zx)
    Zx=fillnas(Zx)
    X=residualize(X,Zx); rm(Zx)
  }
  if(!is.null(Zy))   {
    Zy=convert2dummies(Zy)
    Zy=fillnas(Zy)
    Y=residualize(Y,Zy); rm(Zy)
  }
  
  
  Xnames = dimnames(X)[[2]]
  Ynames = dimnames(Y)[[2]]
  ind.names = dimnames(X)[[1]]
  
  X=scale(X,scale=F)
  svx=svd(X);
  Y=scale(Y,scale=F)
  svy=svd(Y);
  if(any(c(svx$d < 1e-12), (svy$d < 1e-12))){
    Xred=svx$u[,svx$d>1E-12]%*%diag(svx$d[svx$d>1E-12])
    rotX=svx$v[,svx$d>1E-12]
    Yred=svy$u[,svy$d>1E-12]%*%diag(svy$d[svy$d>1E-12])
    rotY=svy$v[,svy$d>1E-12]
    
    mod=CCA::cc(Xred,Yred)
    
    mod$names$Xnames=Xnames
    mod$xcoef=rotX%*%mod$xcoef
    
    mod$names$Ynames=Ynames
    mod$ycoef=rotY%*%mod$ycoef
    
    mod$scores$corr.Y.xscores=cor(Y,mod$scores$xscores)
    mod$scores$corr.Y.yscores=cor(Y,mod$scores$yscores)
    mod$scores$corr.X.xscores=cor(X,mod$scores$xscores)
    mod$scores$corr.X.yscores=cor(X,mod$scores$yscores)
  } else mod=CCA::cc(X, Y) 
  
  return(mod)
}


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

residualize <- function(Y,Z){
  HY=Z%*%solve(t(Z)%*%Z)%*%t(Z)%*%Y
  Y-HY
}

fillnas <- function(Y){
  nas=which(is.na(Y),arr.ind = TRUE)
  if(nrow(nas)==0) return(Y)
  Y[nas]=colMeans(Y[,nas[,2],drop=FALSE],na.rm=TRUE)
  Y
}
