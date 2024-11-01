make_histogram <- function(top){
  hist(top$PValue, breaks = 20)
  plot_obj <- recordPlot()
}
