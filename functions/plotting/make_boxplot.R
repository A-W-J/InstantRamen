make_boxplot <- function(data){
  colors <- brewer.pal(9, "Set1")
  boxplot(log2(data + 1), las = 2, col = "green")
  plot_obj <- recordPlot()
}
