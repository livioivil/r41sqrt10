#' @title Setting for computing scales of a few tests
#' @param X NULL
#' @param grps NULL 
#' @param title "Means"
#' @param col.palette NULL
#' @param na.rm TRUE
#' @param filename NULL
#' @param sort.by "order.vars=order(colMeans(X))"
#' @param means.cis NULL
#' @description barre d'errore
#' @export
#' @examples
#' \dontrun{
#' plt=barplot_errs(dat,col.palette=wes.palette(2, "Darjeeling"),grps=dati$pz.,title=title,sort.by=sort.by)     
#' ps.cor=apply(dat,2,function(x)cor.test(dati$pz,x)$p.value)
#' signif =names(which(p.adjust(ps.cor,"holm")<=.05))
#' plt=plt+ annotate("text", x = signif, y = -.12, label = "*",color="darkred",size=15)
#' plt
#' }

barplot_errs <- function(X=NULL, grps=NULL, 
                         title="Means",
                     col.palette=NULL,  na.rm=TRUE,
                     filename=NULL,sort.by="order.vars=order(colMeans(X))",
                     means.cis=NULL,ylab="means",xlab="",
                     grp.names="grps") {

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
levs=levels(factor(grps))
# levs=levs[length(levs):1]
df$grps=factor(df$grps,levels=levs)
df$x=colnames(res[[1]])
}


if(length(unique(df$grps))<=1) {
  show.legend=FALSE
} else show.legend=TRUE


if(!is.null(sort.by))
  if(is.character(sort.by))
    eval(parse(text=sort.by)) else order.vars=sort.by
if(!all(is.finite(sort.by))) order.vars=1:nvars

if(exists("order.vars")) df$x=factor(df$x,levels=names(X)[order.vars])

library(ggplot2)
if(is.null(col.palette)) col.palette=wesanderson::wes_palette(name = "Darjeeling",type = "continuous",n=length(unique(df$grps)))
# Use 95% confidence intervals instead of SEM

plt2=ggplot2::ggplot(df, aes(x=x, y=Mean,fill=grps, order.vars = as.numeric(grps))) + 
  geom_bar(position=position_dodge(), stat="identity") +
  scale_fill_manual(values=col.palette,guide = guide_legend(title = grp.names))+#coord_flip()+
  geom_errorbar(aes(ymin=CI.inf, ymax=CI.sup),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
# plt2=plt2+geom_text(aes(label = grps), size = 2.5)

#+guide_legend(title ="ciao")
# plt2=plt2+ annotate("text", x =df$x[signif], y = .0, label = "*",color="black")
plt2 = plt2+ scale_y_continuous(ylab) + scale_x_discrete(xlab) + ggtitle(title)
if(!show.legend) {
  plt2=plt2+guides(fill=FALSE)
}
plt2
}