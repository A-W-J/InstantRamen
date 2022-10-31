
plot_wrapper <- function(data, ctl, trt, cds, top, toggle){
  if(toggle == "Bar"){
    make_barplot(data)
  }
  if(toggle == "Box"){
    make_boxplot(data)
  }
  if(toggle == "BCV"){
    make_bcv_plot(cds)
  }
  if(toggle == "Mean-Var"){
    make_meanvar_plot(cds)
  }
  if(toggle == "Histogram"){
    make_hist(top)
  }
  if(toggle == "Smear"){
    make_smear_plot(cds, ctl, trt, top)
  }
}
