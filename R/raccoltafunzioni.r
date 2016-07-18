calcola.errore <- function(predicted, observed){
  
  tab=table(predicted,observed)
  if(ncol(tab)<2) return(list(tabellaConfusione=tab,errore=NA,ErroreRelativo=NA))
  if(nrow(tab)<2) {
    TAB=as.table(matrix(0,2,2))
    dimnames(TAB)[[1]]=dimnames(TAB)[[2]]=dimnames(tab)[[2]]
    TAB[unique(predicted),]=tab
    tab=TAB
  }
  e=  (tab[1,2]+tab[2,1])/(length(observed))
  erel= (tab[1,2]+tab[2,1])/min(table(observed))
  list(tabellaConfusione=tab,errore=e,ErroreRelativo=erel)
}


# 
# #####################################################################
# ### Test Indipendenza via permutazione per lda, qda, glm, vglm e rpart
# ## H0: Le x non sono discriminanti.
# ## H1: Le x hanno potere discriminante.
# perm.test <- function(obj,data,B,rpart.test=NULL){
# 
#   if (missing(data)) {
#       if(is(obj,"vglm")) {
#       data <- as.list(attributes(obj@misc$formula)$.Environment)[names(as.list(attributes(obj@misc$formula)$.Environment))==obj@call[["data"]]][[1]]  
# 		yid <- as.character(obj@call[[2]][[2]])
# 		}
# 	  else { 
# 	    data <- as.list(attributes(obj$term)$.Environment)[names(as.list(attributes(obj$term)$.Environment))==obj$call[["data"]]][[1]]  
# 		yid <- as.character(obj$call[[2]][[2]])
# 		}
# 	}
#     
#   n <- dim(data)[1]
#   Bspace <- matrix(c(1:n,sapply(rep(n,B),sample)),nrow=n)
#   d <- data
#   
#   ##utile solo se is(obj,"rpart")==TRUE
#   rpart.test <- match.arg(rpart.test,c("rel error", "xerror", "minxerror"))
#   if(rpart.test=="minxerror"){
#     rows="all"
#     test.type= "xerror"
#   } else {
#     rows="last"
# 	test.type= rpart.test
# 	}
# 
#   ### crea la funzione che calcola la statistica test in funzione dei dati d
#   if (is(obj,"qda") || is(obj,"lda") ) { statest <- function() mean((predict(update(obj,data=d))$class==d[,yid]))}
#   else  
#   if (is(obj,"glm")) statest <- function() mean(((predict(update(obj,data=d))>0)*1)==d[,yid]) 
#   else
#   if(is(obj,"vglm")) statest <- function() {obj@call[["data"]]=d; mean(levels(d[,yid])[apply(predict( eval(obj@call), type = "response"), 1, which.max)]==d[,yid])}
#   else  
#   if(is(obj,"rpart")) { if (rpart.test=="minxerror") { statest <- function() { temp <- update(obj,data=d)$cptable;
# 					  						    if (is.na(match("xerror",colnames(temp)))) return(0)
# 												else										
#                                                   return(1-min(temp[1:dim(temp)[1],"xerror"]))}
# 												  }
# 					    else if(rpart.test=="xerror") { statest <- function() { temp <- update(obj,data=d)$cptable;
# 					  						    if (is.na(match("xerror",colnames(temp)))) return(0)
# 												else										
#                                                   return(1-temp[dim(temp)[1],"xerror"])}
# 												  }
# 						else {                          statest <- function() { temp <- update(obj,data=d)$cptable;
# 					  						    if (is.na(match("rel error",colnames(temp)))) return(0)
# 												else										
#                                                   return(1-temp[dim(temp)[1],"rel error"])}
# 												   }
# 						}
#   else statest <- NULL
# 
# 
#   Tobs <- statest()  
#   T <- rep(NA,B)
#   T[1] <- Tobs
#   for (i in 2:B){
#     d[,yid] <- d[Bspace[,i],yid]
# 
#     T[i] <- eval(statest(), environment(d))
#   }
#   p <- mean(T>=Tobs)
#   return(list(p=p,Tobs=Tobs,T=T))
# }
# 


##############################################################################
## funzione di Cross Validazione per oggetti di tipo lda, qda, glm e vglm
## se num.valid è tra 0 e 1 (/è un numero intero), rappresenta la proporzione (/il numero assoluto) di osservazioni da usare per la validazione ad ogni passo; num.valid=1 (default) indica la procedura leav-one-out.

xval <- function(obj,data,num.valid=1){
  # data <- as.list(attributes(obj$term)$.Environment)[names(as.list(attributes(obj$term)$.Environment))==obj$call[[3]]][[1]]
  
  
  ### crea la funzione che calcola l'errore sui dati di validazione
  if(is(obj,"qda")||is(obj,"lda")) {
    calc.errore <- function(train,valid) table(valid[,yid],(predict(update(obj,data=train),newdata=valid)$class)) 
    yid <- as.character(obj$call[[2]][[2]])
  }
  else
    if (is(obj,"glm"))  {
      predici <- function(train,valid) predict(update(obj,data=train),newdata=valid)
      calc.errore <- function(train,valid) table(valid[,yid],factor(predict(update(obj,data=train),newdata=valid)>0,levels=c(FALSE,TRUE)))
      yid <- as.character(obj$call[[2]][[2]])
    }
  else
    if (is(obj,"vglm")) {
      yid <- as.character(obj@call[[2]][[2]])
      calc.errore <- function(train,valid) {
        newfit <- vglm(formula=obj@call[["formula"]],data=train,family=multinomial())
        table(valid[,yid],factor(levels(data[,yid])[apply(predict(newfit,newdata=valid, type = "response"), 1, which.max)],levels=levels(data[,yid])))
      }
    }
  else 	return()
  
  data[,yid]=factor(data[,yid])
  n <- dim(data)[1]
  
  
  B = n %/% num.valid+((n %% num.valid)>0)
  riordinamento <- sample(1:n,replace=FALSE)
  vuoto=rep(NA,n)
  names(vuoto)=rownames(data)
  
  mat.conf=as.table(matrix(0,nlevels(factor(data[,yid])),nlevels(factor(data[,yid]))))
  dimnames(mat.conf)= list(reali=levels(data[,yid]),previsti=levels(data[,yid]))
  res=lapply(1:B,function(i){
    training=data[riordinamento[-((num.valid*(i-1)+1):min(n,(num.valid*i)))],,drop=FALSE]
    validation=data[riordinamento[(num.valid*(i-1)+1):min(n,(num.valid*i))],,drop=FALSE]
    list(mat.conf=mat.conf+calc.errore(train=training,valid=validation),
         predetti=predici(train=training,valid=validation))
  })

  mat.conf=rowSums(sapply(res,function(x)x$mat.conf))
  mat.conf=as.table(matrix(mat.conf,nlevels(factor(data[,yid])),nlevels(factor(data[,yid]))))
  xerr=1-sum(diag(mat.conf))/sum(mat.conf)
  previsioni=unlist(sapply(res,function(x)x$predetti))
  
  vuoto[riordinamento]=previsioni
  
  invisible(list(predicted_cv=previsioni,mat.conf=mat.conf,xerr=xerr,
       relxerr=xerr/((n-max(table(data[,yid])))/n)))
}


######################################
devTBW <- function (X,class)
  #
  # computes Total (T), Between (B) and Within (W) deviances
  #
{
  n<-length(X[,1])
  DevT<-(n-1)*cov(X)
  DevB<-(n-1)*cov(apply(X,2,function(x){FUN=ave(x,class,FUN=mean)}))
  DevW<-DevT-DevB
  return(list(DevT=DevT,DevB=DevB,DevW=DevW))
}
###################################################
hclust.summary <- function (X,hc)
  #
  # computes pseudo F and T^2
  #
{
  n<-length(X[,1])
  td<-sum(diag(var(X)))*(n-1)
  wdold<-td
  ncsold<-rep(n,n)
  ones<-rep(1,n)
  outhcs<-cbind(hc$merge,rep(0,n-1),rep(0,n-1))
  rownames(outhcs)<-seq(n-1,1,-1)
  colnames(outhcs)<-c("c1","c2","pT^2","pF")
  for(g in 2:(n-1)){
    memb<-cutree(hc,k=g)
    outp<-lm(as.matrix(X)~factor(memb))
    bd<-sum(diag(var(outp$fitted.values)))*(n-1)
    wd<-td-bd
    pF<-(bd/(g-1))/(wd/(n-g))
    outhcs[n-g,4]<-pF
    #
    ncs<-ave(ones,factor(memb),FUN=sum)
    nc<-sum(ncsold!=ncs)
    pT2<-(wdold-wd)/(sum(outp$residuals[ncsold!=ncs,]^2)/(nc-2))
    outhcs[n-g+1,3]<-pT2
    #
    wdold<-wd
    ncsold<-ncs
  }
  print(outhcs)
}
################################
##praticamente equivalente a: anova(manova(as.matrix(Y)~1))
hotelling.test <- function(Y,m0=0)  
{# hotelling test per la verifica d'ipotesi H0: mu=m0
  # con sigma ignota e stimata dai dati
  # Y matrice o data.frame n x p, m0 vettore di lunghezza p o scalare (il valore verrà replicato)
  n<-nrow(Y)
  p<-ncol(Y)
  
  M <- var(Y)/n  #varianza empirica delle media campionaria, quindi M*(n-1)*n ~ W(Sigma,n-q)
  d <- colMeans(Y)-m0  #normale N(B,M)
  T2=t(d)%*%solve(M)%*%d #T2(p,n-rank.x) distrib p/(m-p+1)F(p,m-p+1)=p/(n-q-p+1)F(p,n-q-p+1)
  
  df.num<-p
  df.den<-n-p
  statistic<-T2*(n-p) /(n-1)/p
  pvalue<-1-pf(statistic,df.num,df.den)
  data.frame(row.names="T2-Hotelling",T2=T2,F.statistic= statistic,df.num=df.num,df.den=df.den,pvalue=pvalue)
}

############################
############################
# funzione che calcola la soluzione lda sulla base di medie dei due campioni e varianza (e prob a priori)
# per i vostri esercizi con carta e penna..
classificlda<-function(x,mu1,mu2,sigma,pi=c(2/3,1/3)){
  log(pi[1]/pi[2])+t(x)%*%solve(sigma)%*%(mu1-mu2) - 1/2*(t(mu1)%*%solve(sigma)%*%mu1 - t(mu2)%*%solve(sigma)%*%mu2)
}

# funzione che calcola la soluzione qda sulla base di medie dei due campioni e varianza (e prob a priori)
# per i vostri esercizi con carta e penna..
classificQda<-function(x,mu1,mu2,sigma1,sigma2,pi=c(2/3,1/3)){
  c( QDF1=log(pi[1])+length(mu1)/2* log(det(sigma1)) - 1/2*t(x-mu1)%*%solve(sigma1)%*%(x-mu1), 
     QDF2=log(pi[2])+length(mu2)/2* log(det(sigma2)) - 1/2*t(x-mu2)%*%solve(sigma2)%*%(x-mu2))
}
