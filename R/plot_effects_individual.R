#' @name plot_effects_individual
#' @title plot_effects_individual
#' @author livio finos
#' @param data a data.frame
#' @param pred_name character. name of the predictor (x)
#' @param resp_name character. name of the response (y)
#' @param predict_funct a function that makes predictions
#' @param col.by NULL or a vector of values that indicate the strata
#' @param npoints 10
#' @param center_effs logical. Should be the effect of the oder predictors removed from the lines and the observations? 
#' @export
#' @import ggplot2
#' @examples
#' n=100
#' X=matrix(rnorm(n*3),n,3)
#' X[,2]=sign(X[,2])
#' y=rnorm(n,X[,1]+X[,1]*X[,2])
#' D=data.frame(X)
#' D$y=y
#' 
#' # Regression model
#' mod=lm(y~X1*X2+X3,data=D)
#' summary(mod)
#' 
#' predict_funct=function(newdata) predict(mod,newdata=newdata)
#' plot_effects_individual(D,"X1","y",predict_funct=predict_funct)
#' plot_effects_individual(D,"X1","y",predict_funct=predict_funct,col.by = D$X2)
#' plot_effects_individual(D,"X3","y",predict_funct=predict_funct,center_effs = TRUE)
#' plot_effects_individual(D,"X3","y",predict_funct=predict_funct,center_effs = FALSE)
#' 
#' # Regression tree model
#' \dontrun{
#' require(rpart)
#' mod=rpart(y~X1+X2+X3,data=D,control = list(cp=.0001))
#' print(mod)
#' printcp(mod)
#' 
#' predict_funct=function(newdata) predict(mod,newdata=newdata)
#' #plot_effects_individual(D,"X1","y",predict_funct=predict_funct)
#' plot_effects_individual(D,"X1","y",predict_funct=predict_funct,col.by = D$X2)
#' # compare the two:
#' plot_effects_individual(D,"X3","y",predict_funct=predict_funct,center_effs = TRUE)
#' plot_effects_individual(D,"X3","y",predict_funct=predict_funct,center_effs = FALSE)
#' }

plot_effects_individual <- function(data,pred_name,resp_name,predict_funct,
                         col.by=NULL,npoints=10,center_effs=TRUE){
  rownames(data)=NULL
  if(is.numeric(data[,pred_name])) {
    rng=range(data[,pred_name])
    if(any(is.na(rng))) warning("NA are not allowed!")
    pred_values=seq(from=rng[1],to=rng[2],length.out = npoints)
  } else {
    pred_values=unique(data[,pred_name])
  }
  temp=data
  temp$pred_var=temp[,pred_name]
  temp[,pred_name]=mean(data[,pred_name])
  
  pred_id=grep(pred_name,colnames(data))

  mat_lines=plyr::llply(1:nrow(data),make_line_pred,
                        pred_id=pred_id,data=data,
                        pred_values=pred_values,
                        pred_name=pred_name,
                        center_effs=center_effs,
                        predict_funct=predict_funct)
  # names(data)=gsub(resp_name,"resp_var",names(data))
  # mat_lines
    if(is.null(col.by)) temp$col.by=factor(1) else
    temp$col.by=factor(col.by)
  
  if(center_effs) {
    centers=plyr::laply(mat_lines,function(x){attr(x,"scaled:center")})
  } else {
      centers=rep(0,nrow(data))
    }
  names(mat_lines)=1:length(mat_lines)
  mat_lines=as.data.frame(mat_lines)
  mat_lines=t(mat_lines)
  
  if(center_effs)   
    temp$nett_resp=unlist(data[resp_name])-predict_funct(temp) else
      temp$nett_resp=unlist(data[resp_name])
  # names(temp)=gsub(pred_name,"pred_var",names(temp))
  
  #ggplot needs a dataframe
  mat_lines <- as.data.frame(mat_lines)
  #id variable for position in matrix 
  mat_lines$id <- 1:nrow(mat_lines) 
  #reshape to long format
  plot_data <- reshape2::melt(mat_lines,id.var="id")
  plot_data$col.by=temp$col.by
  plot_data$pred_var=rep(pred_values,each=nrow(data))
  pp=ggplot2::ggplot()+ ggplot2::geom_line(data=plot_data, 
                     aes(x=pred_var,y=value,group=id,colour=col.by))
  pp=pp+ theme(legend.position="none")+labs(x=pred_name,y=resp_name)
  pp=pp+geom_point(data=temp, aes(x=pred_var,y=nett_resp,colour=col.by))
   pp
}

############
make_line_pred <- function(i,pred_id,data,pred_values,pred_name,center_effs,predict_funct){
  newdata=cbind(pred_values,data[i,-pred_id,drop=FALSE],row.names=NULL)
  colnames(newdata)[1]=pred_name
  newdata=newdata[,c(2:pred_id,1,(pred_id+1):ncol(newdata))]
  pred_vals=predict_funct(newdata)
  pred_vals=scale(pred_vals,scale = FALSE,center = center_effs)
  pred_vals
}

