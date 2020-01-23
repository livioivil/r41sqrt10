#'@export
#'@alias webplot.multi
#'@alias webplot
webplot.multi <- function(data, y.cols = NULL, main = NULL, add = F, 
                          scale=TRUE,lwd=2,lty=1,cex=1,
         col=NULL,...){
# par(mar = c(1, 1, 2, 1))
  
if((!exists("col"))||(is.null(col))){
  col=palette()
  }
data.rows=rownames(data)
  nrows=nrow(data)
webplot(data, data.rows[1], main = main, col = col[1], lwd=lwd,lty=lty,cex=cex,
        scale=scale,...)
for (i in 2:nrows)
  webplot(data, data.rows[i], add = T, col = col[i], lwd=lwd,lty=lty,cex=cex,
          scale=scale,...)
par(new = TRUE)
par(mar = c(0, 0, 0, 0))
plot(0, type = "n", axes = F)
legend("bottomright", legend=rownames(data), 
       col = col[1:nrows], rownames, bty = "n",
         cex=cex,
       lty = lty, lwd = lwd, ...       )
}
