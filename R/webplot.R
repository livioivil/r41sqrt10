#' webplot
#' #' 
#' @aliases webplot webplot.multi
#' @param data to be done %% ~~Describe \code{data} here~~
#' @param data.row to be done%% ~~Describe \code{data.row} here~~
#' @param y.cols to be done%% ~~Describe \code{y.cols} here~~
#' @param main to be done%% ~~Describe \code{main} here~~
#' @param add to be done%% ~~Describe \code{add} here~~
#' @param col to be done%% ~~Describe \code{col} here~~
#' @param scale if \code{TRUE}: data = apply(data, 2, function(x) ((x -
#' min(x))/(max(x)-min(x))+.1)/1.1)
#' @param col  as in \code{plot()}
#' @param lwd  as in \code{plot()}
#' @param lty  as in \code{plot()}
#' @param cex  as in \code{plot()}
#' @param \dots to be done %% ~~Describe \code{\dots} here~~
#' @examples
#' data(pal.unipd)
#' 
#' X <- as.data.frame(matrix(rpois(30,100),3,10))
#' webplot(X[1,])
#' webplot.multi(X,col=pal.unipd)
#' 
#' @export webplot

webplot <- function(data, data.row = NULL, y.cols = NULL, main = NULL, add = F, 
                   col = "red", scale = T, lwd=3, lty=1, cex=1,
                   ...) {
  if (!is.matrix(data) & !is.data.frame(data)) 
    stop("Requires matrix or data.frame")
  if (is.null(y.cols)) 
    y.cols = colnames(data)
  if (sum(!sapply(data[, y.cols], is.numeric)) > 0) {
    out = paste0("\"", colnames(data)[!sapply(data, is.numeric)], "\"", 
                 collapse = ", ")
    stop(paste0("All y.cols must be numeric\n", out, " are not numeric"))
  }
  if (is.null(data.row)) 
    data.row = 1
  if (is.character(data.row)) 
    if (data.row %in% rownames(data)) {
      data.row = which(rownames(data) == data.row)
    } else {
      stop("Invalid value for data.row:\nMust be a valid rownames(data) or row-index value")
    }
  if (is.null(main)) 
    main = rownames(data)[data.row]
  if (scale == T) {
#     data = scale(data[, y.cols])
    if(nrow(data)==1) 
      data=data/max(abs(data)) else
        data = apply(data, 2, function(x) ((x - min(x))/(max(x)-min(x))+.1)/1.1)
  }
  data = as.data.frame(data)
  n.y = length(y.cols)
  min.rad = 360/n.y
  polar.vals = (90 + seq(0, 360, length.out = n.y + 1)) * pi/180
  
  # 
  if (add == F) {
    plot(0, xlim = c(-2.2, 2.2), ylim = c(-2.2, 2.2), type = "n", axes = F, 
         xlab = "", ylab = "")
    title(main,...)
    lapply(polar.vals, function(x) lines(c(0, 2 * cos(x)), c(0, 2 * sin(x))))
    lapply(1:n.y, function(x) text(2.15 * cos(polar.vals[x]), 2.15 * sin(polar.vals[x]), 
                                   y.cols[x], cex = cex))
    
    lapply(seq(0.5, 2, 0.5), function(x) lines(x * cos(seq(0, 2 * pi, length.out = 100)), 
                                               x * sin(seq(0, 2 * pi, length.out = 100)), lwd = 0.5, lty = 2, col = "gray10"))
    lines(cos(seq(0, 2 * pi, length.out = 100)), sin(seq(0, 2 * pi, length.out = 100)), 
          lwd = 1.2, col = "gray50")
  }
  
  
  r = 2* data[data.row, y.cols]
  xs = -r * cos(polar.vals)
  ys = r * sin(polar.vals)
  xs = c(xs, xs[1])
  ys = c(ys, ys[1])
  
  lines(xs, ys, col = col, lwd = lwd, lty = lty,cex=cex,...)
}
