#'@title summaryResSim
#'@name summaryResSim
#'@export
summaryResSim <- function(res,alphas=c(.01,.05,.1,.5,.75),na.rm=FALSE){
  out=sapply(alphas,function(a)colMeans(res<=a,na.rm=na.rm))
  if(!is(out,"matrix")) out=t(matrix(out))
  colnames(out)=paste("<=",alphas,sep="")
out  
}
######
#'@name plotResSim
#'@title plotResSim
#'@param resSim matrix Bxp with p number of test, B number of replications
#'@param legend logical, names are taken from the colnames of resSim
#'@export
plotResSim <- function(resSim, nameOut=NULL, title="Type I Error Control",cols=NULL, alsoZoom=FALSE,
                       width=10,height=10,na.rm=FALSE,col.abline="red",verticals=FALSE,legend=TRUE,...){
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
  if(legend) legend("bottomright", legend=colnames(resSim),col=cols,lwd=5,
                    bty="n",cex=3)
  sapply(1:ncol(resSim),
         function(i) plot.ecdf(.getRes(resSim[,i],na.rm=na.rm),col=cols[i],add=TRUE,lwd=8,verticals = verticals,cex=3))
  abline(0,1,col=col.abline)
  if(!is.null(nameOut)) dev.off()
  
  if(alsoZoom) {
    if(!is.null(nameOut)) pdf(paste(nameOut,"_ZOOM.pdf",sep=""),width=width,height=height)
    Ecdf(resSim[,1],ylab="proportion of rejections",
         xlab="Significance level",main=title,col="white",
         xlim=c(-.1,.3),ylim=c(0,.35),...)
    sapply(1:ncol(resSim),
           function(i) Ecdf(resSim[,i],col=cols[i],add=TRUE,lwd=8))
    abline(0,1,col="red")
    if(legend) legend(.4,.7, legend=colnames(resSim),col=cols,lwd=5,
           bty="n",cex=sqrt(1/ncol(resSim)))
    if(!is.null(nameOut)) dev.off()
  }
}


#' @export
plotResSim_ <- function(resSim, title="Proportion of Rejections",
                        ylab=NULL){
  if(is.null(ylab)) ylab=title
  df <- as.data.frame.table(resSim)
  df$Var1=NULL
  names(df)=c("Method","Rejections")
  ggplot(df, aes(Rejections, colour = Method)) + stat_ecdf() + geom_abline(intercept = 0,slope = 1,colour="black",alpha = 0.3)+ 
    theme_bw()+ggtitle("")+xlab("Signif. Level") + ylab(ylab)+ggtitle(title)
}
#   , nameOut=NULL, title="Type I Error Control",cols=NULL, alsoZoom=FALSE,
#                        width=10,height=10,na.rm=FALSE,col.abline="red",verticals=FALSE,legend=TRUE,...){
#   #   if(is.null(cols)) cols=wes.palette(name = "Zissou", type = "continuous")(ncol(resSim))
#   if(is.null(cols)) cols=rainbow(ncol(resSim))
#   if(is.null(nameOut)) if(alsoZoom) par(mfrow=c(1,2)) # else par(mfrow=c(1,1))
#   if(is.null(colnames(resSim))) colnames(resSim)=1:ncol(resSim)
#   .getRes <- function(x,na.rm){
#     if(na.rm) x=x[!is.na(x)] 
#     x
#   }
#   
#   idout=which(apply(resSim,2,function(x)all(is.na(x))))
#   if(length(idout)>0) resSim <- resSim[,-idout]
#   
#   if(!is.null(nameOut)) pdf(paste(nameOut,".pdf",sep=""),width=width,height=height) else if(alsoZoom) par(mfrow=c(1,2))
#   plot.ecdf(.getRes(resSim[,1],na.rm=na.rm),ylab="proportion of rejections",xlab="Significance level",main=title,col="white",
#             xlim=c(0,1),ylim=c(0,1),asp=1,lwd=8,cex.lab=2,cex.axis=2)
#   if(legend) legend("bottomright", legend=colnames(resSim),col=cols,lwd=5,
#                     bty="n",cex=3)
#   sapply(1:ncol(resSim),
#          function(i) plot.ecdf(.getRes(resSim[,i],na.rm=na.rm),col=cols[i],add=TRUE,lwd=8,verticals = verticals,cex=3))
#   abline(0,1,col=col.abline)
#   if(!is.null(nameOut)) dev.off()
#   
#   if(alsoZoom) {
#     if(!is.null(nameOut)) pdf(paste(nameOut,"_ZOOM.pdf",sep=""),width=width,height=height)
#     Ecdf(resSim[,1],ylab="proportion of rejections",
#          xlab="Significance level",main=title,col="white",
#          xlim=c(-.1,.3),ylim=c(0,.35),...)
#     sapply(1:ncol(resSim),
#            function(i) Ecdf(resSim[,i],col=cols[i],add=TRUE,lwd=8))
#     abline(0,1,col="red")
#     if(legend) legend(.4,.7, legend=colnames(resSim),col=cols,lwd=5,
#                       bty="n",cex=sqrt(1/ncol(resSim)))
#     if(!is.null(nameOut)) dev.off()
#   }
# }