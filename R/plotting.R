#'@export
#'@name plotP
#'@title plotP
plotP <- function(x,y,...){
  rx=range(x)
  dx=rx[2]-rx[1]
  rx=rx+(c(-dx/10,+dx/10))
  ry=range(y)
  dy=ry[2]-ry[1]
  ry=ry+(c(-dy/10,+dy/10))
  plot(x,y,bg=heat.colors(length(x)),xlim=rx,ylim=ry,...,cex.main=1.5,lwd=2,col="darkgrey",pch=21,cex=2)
  abline(h=0)
  abline(v=0)
}	


#'@export
#'@title plotLog10
#'@name plotLog10
plotLog10 <- function(x,y,...){
  plot(-log10(x),-log10(y),...); abline(0,1)}


#'@title logit
#'@name expit
#'@export logit expit
logit <- function(x) x/(1-x)
expit <- function(x) exp(x)/(1+exp(x))
