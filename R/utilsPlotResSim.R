library(Hmisc)
summaryResSim <- function(res,alphas=c(.01,.05,.1,.5,.75),na.rm=FALSE){
  out=sapply(alphas,function(a)colMeans(res<=a,na.rm=na.rm))
  colnames(out)=paste("<=",alphas,sep="")
out  
}
######
plotResSim <- function(resSim, nameOut=NULL, title="Type I Error Control",cols=NULL, alsoZoom=FALSE,
                       width=10,height=10,na.rm=FALSE,col.abline="red",verticals=FALSE,...){
#   if(is.null(cols)) cols=wes.palette(name = "Zissou", type = "continuous")(ncol(resSim))
  if(is.null(cols)) cols=rainbow(ncol(resSim))
  if(is.null(nameOut)) if(alsoZoom) par(mfrow=c(1,2)) # else par(mfrow=c(1,1))
  if(is.null(colnames(resSim))) colnames(resSim)=1:ncol(resSim)
  .getRes <- function(x,na.rm){
    if(na.rm) x=x[!is.na(x)] 
    x
  }
  
  idout=which(apply(resSim,2,function(x)all(is.na(x))))
  if(length(idout)>0) resSim <- resSim[,-idout]
  
  if(!is.null(nameOut)) pdf(paste(nameOut,".pdf",sep=""),width=width,height=height) else if(alsoZoom) par(mfrow=c(1,2))
  plot.ecdf(.getRes(resSim[,1],na.rm=na.rm),ylab="proportion of rejections",xlab="Significance level",main=title,col="white",
            xlim=c(0,1),ylim=c(0,1),asp=1,lwd=8,cex.lab=2,cex.axis=2)
  sapply(1:ncol(resSim),
         function(i) plot.ecdf(.getRes(resSim[,i],na.rm=na.rm),col=cols[i],add=TRUE,lwd=8,verticals = verticals,cex=3))
  abline(0,1,col=col.abline)
  legend("bottomright", legend=colnames(resSim),col=cols,lwd=5,
         bty="n",cex=3)
  if(!is.null(nameOut)) dev.off()
  
  if(alsoZoom) {
    if(!is.null(nameOut)) pdf(paste(nameOut,"_ZOOM.pdf",sep=""),width=width,height=height)
    Ecdf(resSim[,1],ylab="proportion of rejections",
         xlab="Significance level",main=title,col="white",
         xlim=c(-.1,.3),ylim=c(0,.35),...)
    sapply(1:ncol(resSim),
           function(i) Ecdf(resSim[,i],col=cols[i],add=TRUE,lwd=8))
    abline(0,1,col="red")
    legend(.4,.7, legend=colnames(resSim),col=cols,lwd=5,
           bty="n",cex=sqrt(1/ncol(resSim)))
    if(!is.null(nameOut)) dev.off()
  }
}