make_meanvar_plot <- function(cds){
  plotMeanVar(cds,
                      show.raw.vars = TRUE,
                      show.tagwise.vars = TRUE,
                      show.binned.common.disp.vars = FALSE,
                      show.ave.raw.vars = FALSE,
                      NBline = TRUE,
                      nbins = 100,
                      pch = 16,
                      xlab = "Mean Expression (log10 scale)",
                      ylab = "Variance (log10 scale",
                      main = "Mean-Variance Plot")
  plot_obj <- recordPlot()
}
