#' @title Setting for computing scales of a few tests
#' @param X NULL
#' @param grps NULL 
#' @param title "Means"
#' @param col.palette NULL
#' @param na.r TRUE
#' @param filename NULL
#' @param sort.by "order=order(colMeans(X))"
#' @param means.cis NULL
#' @description barre d'errore
#' @export
#' @example
#' grafici.barre <- function(dat,title,sort.by=TRUE){
#' plt=barplot.errs(dat,col.palette=wes.palette(2, "Darjeeling"),grps=dati$pz.,title=title,sort.by=sort.by)     
#' ps.cor=apply(dat,2,function(x)cor.test(dati$pz,x)$p.value)
#' signif =names(which(p.adjust(ps.cor,"holm")<=.05))
#' plt=plt+ annotate("text", x = signif, y = -.12, label = "*",color="darkred",size=15)
#' plt
#' }
#' 
barplot_errs <- function(X=NULL, grps=NULL, 
                         title="Means",
                     col.palette=NULL,  na.rm=TRUE,
                     filename=NULL,sort.by="order=order(colMeans(X))",
                     means.cis=NULL) {

if(is.null(X)){
  df=means.cis
  rm(means.cis)
  nvars=length(unique(df$x))
} else {
  if(is.null(grps)) grps=rep(1,nrow(X))    
  nvars=ncol(X)
 res=by(X,grps, function(D) (stats(D,c("mean","CI"))))

df=c()
for(i in 1:length(res))
  df=rbind(df,t(res[[i]]))
rownames(df)=NULL
df=data.frame(df)
df$grps=rep(names(res),each=ncol(res[[1]]))
df$grps=factor(df$grps,levels=levels(factor(grps)))
df$x=colnames(res[[1]])
}


if(length(unique(df$grps))<=1) {
  show.legend=FALSE
} else show.legend=TRUE


if(!is.null(sort.by))
  eval(parse(text="order=order(colMeans(X))")) else order=1:nvars
df$x=factor(df$x,levels=names(X)[order])

library(ggplot2)
library(wesanderson)
if(is.null(col.palette)) col.palette=wes.palette(name= "Darjeeling",type = "continuous")(length(unique(df$grps)))
# Use 95% confidence intervals instead of SEM

plt2=ggplot2::ggplot(df, aes(x=x, y=Mean,fill=grps, order = as.numeric(grps))) + 
  geom_bar(position=position_dodge(), stat="identity") +
  scale_fill_manual(values=col.palette)+coord_flip()+
  geom_errorbar(aes(ymin=CI.inf, ymax=CI.sup),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
# plt2=plt2+ annotate("text", x =df$x[signif], y = .0, label = "*",color="black")
plt2 = plt2+ scale_y_continuous("means") + scale_x_discrete("") + ggtitle(title)
if(!show.legend) {
  plt2=plt2+guides(fill=FALSE)
}
plt2
}