
###################
#' @example #by.df(dati,gruppo,function(dati)round(t(stats(dati,c("mean","sd"))),2))
#' @aliases tableFrPrc
#' @name stats
#' @title descriptive stats functions
#' @export
#' 

stats= function(x,stat=c("mean","sd","valids","not.valids","quantile","CI","mse"),
                as.vector=FALSE){
  stat= match.arg(stat,c("mean","sd","valids","not.valids","quantile","CI","mse"),several.ok=TRUE)
  
  if((ncol(x)>1)&&(!is.null(ncol(x)))) {
    res=apply(x,2,stats,stat)   
  }    else    {
      invalids=sum(is.na(x));
      Sd=if (any(c("sd","CI","mse")%in%c(stat))) sd(x,na.rm=TRUE) else NULL
      Mean=if(any(c("mean","CI")%in%c(stat)))  mean(x,na.rm=TRUE) else NULL
      
      if("CI"%in%c(stat)) {
        CI =Mean+c(-1,+1)*Sd*qnorm(.975)/sqrt(sum(!is.na(x)))
        names(CI)=c("inf","sup")
      } else CI = NULL
      if(("mse"%in%stat)) Mse=Sd/sqrt(sum(!is.na(x))) else Mse=NULL
      if(!("mean"%in%stat))Mean=NULL
      if(!("sd"%in%stat))Sd=NULL  
    	res=c(Mean=Mean,
    		Sd=Sd,
        Mse=Mse,
    		Valids=if("valids"%in%c(stat)) length(x)-invalids else NULL,
    		Missing=if("not.valids"%in%c(stat))invalids else NULL,
    		CI=if("CI"%in%c(stat)) CI  else NULL) 
      if("quantile"%in%c(stat)){
        Q=quantile(x, probs = seq(0, 1, 0.25),na.rm=TRUE)
        names(Q)=c("min","Qrtl.I","median","Qrtl.III","max")
        res=c(res,Q)
      }
    	res	
    }
  res
}


# strata.summary <- function(y,strata){
# 	rr=by(y,strata,stats)
# 	attr(rr,"class")="list"
# 	data.frame(rr)
# 
# }
##### sostituita con la piu' generica:
# ##### come by ma restituisce un data.frame
# by.df<-function(...){
#   rr=by(...)
#   attr(rr,"class")="list"
#   data.frame(rr)
# }

tableFrPrc <- function(x,title=NULL,rounding=1){
  tab=table(x)
  tab=rbind(Frq=tab, Perc=prop.table(tab)*100)
  tab=addmargins(tab,2)
  if(!is.null(tab)) names(dimnames(tab))=c("",title)
  tab=round(tab,rounding)
  tab
}

strata.perc <- function(Y,strata,round.digits=1,na.rm=FALSE,test=FALSE,as.table=FALSE){
    if(length(strata)==1){
      if(is.character(strata)) strata.id=colnames(Y)[which(colnames(Y)==strata)] else stata.id=strata
      strata=Y[,strata.id,drop=FALSE]
      Y[,strata.id]=NULL
    }
    rr=(round(by.df(Y,strata,colMeans,na.rm=TRUE)*100,1))
    if(test){
      ps=round(apply(Y,2, function(y) chisq.test(y,strati,simulate.p.value =TRUE)$p.value),3)
      stars=ifelse(ps<.05,"*","")
      ps=ifelse(ps==0,"<.001",ps)
      rr=cbind(rr,p.value=ps,Sign=stars)
    }
    if(as.table){
      rr=as.table(as.matrix(rr))
    }
    rr= apply(rr,2,as.character)
    rr
  }

###########
#'@export
#'@name strata.meanPmSd
#'@description strata.meanPmSd
#'@param Y
#'@param strata
#'@param round.digits 1 
#'@param na.rm FALSE 
#'@param test FALSE
#'@param mse FALSE
#'@param as.table FALSE
#'@param pm TRUE
#'@param addTotal FALSE
#'@param medianAndRange FALSE
strata.meanPmSd <- function(Y,strata,round.digits=1,na.rm=FALSE,test=FALSE,mse=FALSE,as.table=FALSE,pm=TRUE,addTotal=FALSE,
                            medianAndRange=FALSE){
  if(test=="ranks"){ 
    rankTest=TRUE
    test=TRUE
  } else  rankTest=FALSE
  
  if(length(strata)==1){
    if(is.character(strata)) strata.id=colnames(Y)[which(colnames(Y)==strata)] else stata.id=strata
    strata=Y[,strata.id,drop=FALSE]
    Y[,strata.id]=NULL
  }
  
  if(!medianAndRange)
    if(pm){
      
      if(mse){
        meanPmSd <- function(y) apply(y,2,function(y.var) paste(round(mean(y.var,na.rm=na.rm),round.digits),sep=""," $\\pm$ ",
                                                                round(sd(y.var,na.rm=na.rm)/sqrt(sum(!is.na(y.var))),round.digits)))  #&plusmn
      } else {
        meanPmSd <- function(y) apply(y,2,function(y.var) paste(round(mean(y.var,na.rm=na.rm),round.digits),sep=""," $\\pm$ ",
                                                                round(sd(y.var,na.rm=na.rm),round.digits)))  #&plusmn
      }
    } else{
      
      if(mse){
        meanPmSd <- function(y) apply(y,2,function(y.var) paste(round(mean(y.var,na.rm=na.rm),round.digits),sep=""," (",
                                                                round(sd(y.var,na.rm=na.rm)/sqrt(sum(!is.na(y.var))),round.digits),")"))  #&plusmn
      } else {
        meanPmSd <- function(y) apply(y,2,function(y.var) paste(round(mean(y.var,na.rm=na.rm),round.digits),sep=""," (",
                                                                round(sd(y.var,na.rm=na.rm),round.digits),")"))  #&plusmn
      }
    } else #(medianAndrange==TRUE)
    {
      meanPmSd <- function(y) apply(y,2,function(y.var) 
        paste(round(median(y.var,na.rm=na.rm),round.digits),sep=" ",
              paste("[",paste(round(range(y.var,na.rm=na.rm),round.digits),collapse=" - "),"]",sep="")
              )
        )
    }
  
  
  rr=by(Y,strata, meanPmSd)
  attr(rr,"class")="list"
  rr=data.frame(rr)
  if(addTotal)
    rr=cbind(rr,Total=meanPmSd(Y))
  
  if(test){
    if(rankTest){
      ps=round(apply(Y,2, function(y) kruskal.test(y~.,data=strata)$p.value),3) 
    } else
      ps=round(apply(Y,2, function(y) anova(lm(y~.,data=strata))[1,"Pr(>F)"]),3)
    stars=ifelse(ps<.05,"*","")
    ps=ifelse(ps==0,"<.001",ps)
    rr=cbind(rr,p.value=ps,Sign=stars)
  }
  if(as.table){
    rr=as.table(as.matrix(rr))
  }
  rr= apply(rr,2,as.character)
  rownames(rr)=colnames(Y)
  rr
  
}

var.is <- function(data){
  what.is <- function(x){
    if(is.numeric(x)) return("numeric")
    if(is.factor(x)) return("factor")
    if(is.character(x)) return("character")
    "non so"
  }
  out=sapply(1:ncol(data),function(i) what.is(data[,i]))
  names(out)=colnames(data)
  out
}



#####################
#'@export
#'@name frPrc
#'@description frPrc
#'@param x x
#'@param strata strata
#'@param test FALSE
frPrc <- function(x,strata,test=FALSE){
    x=factor(x)
    strata=factor(strata)
    tab0=table(x,strata)
    tab=addmargins(tab0,2)
    perc=round(prop.table(tab,2)*100,1)
    res=NULL
    for(i in 1:ncol(tab))
      res=cbind(res,paste(tab[,i]," (",perc[,i],")",sep=""))
    colnames(res)=c(levels(strata),"Total")
    rownames(res)=levels(x)
    if(test){
      res=cbind(res,sign="")
      p=chisq.test(tab0,simulate.p.value=TRUE,B=5000)$p.value
      res[nrow(res),ncol(res)]=ifelse(p<.001,"<.001",round(p,4))
    }
    res
  }
  