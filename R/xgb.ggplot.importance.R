xgb.ggplot.importance <- function (importance_matrix = NULL, top_n = NULL, measure = NULL, 
                                   rel_to_first = FALSE, n_clusters = c(1:10), ...) 
{
  importance_matrix <- xgb.plot.importance(importance_matrix, 
                                           top_n = top_n, measure = measure, rel_to_first = rel_to_first, 
                                           plot = FALSE, ...)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required", call. = FALSE)
  }
  if (!requireNamespace("Ckmeans.1d.dp", quietly = TRUE)) {
    stop("Ckmeans.1d.dp package is required", call. = FALSE)
  }
  clusters <- suppressWarnings(Ckmeans.1d.dp::Ckmeans.1d.dp(importance_matrix$Importance, 
                                                            n_clusters))
  importance_matrix[, `:=`(Cluster, as.character(clusters$cluster))]
  plot <- ggplot2::ggplot(importance_matrix, ggplot2::aes(x = factor(Feature, 
                                                                     levels = rev(Feature)), y = Importance, width = 0.5), 
                          environment = environment()) + ggplot2::geom_bar(color= "red", 
                                                                           stat = "identity", position = "identity") + ggplot2::coord_flip() + 
    ggplot2::xlab("Features") + ggplot2::ggtitle("Feature importance")  + theme_bw()+ 
    ggplot2::theme(plot.title = ggplot2::element_text(lineheight = 0.9, 
                                                      face = "bold"), panel.grid.major.y = ggplot2::element_blank())
  return(plot)
}