#'cc
#'@description Same as \code{cc()} of package \code{CCA}, but it allows for X and Y to be rank deficient.
#'See \code{\link[CCA]{cc}} for a proper documentation.
#'@export

cc<-function (X,Y) 
{
  Xnames = dimnames(X)[[2]]
  Ynames = dimnames(Y)[[2]]
  ind.names = dimnames(X)[[1]]
  
  X=scale(X,scale=F)
  svx=svd(X);
  Y=scale(Y,scale=F)
  svy=svd(Y);
  if((svx$d<1E-12)||(svy$d<1E-12)){
    Xred=svx$u[,svx$d>1E-12]%*%diag(svx$d[svx$d>1E-12])
    rotX=svx$v[,svx$d>1E-12]
    Yred=svy$u[,svy$d>1E-12]%*%diag(svy$d[svy$d>1E-12])
    rotY=svy$v[,svy$d>1E-12]
    
    mod=CCA::cc(Yred,Xred)
    
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