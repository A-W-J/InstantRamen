make_smear_plot <- function(cds, ctl, trt, top){
  de <- rownames(top[top$FDR < 0.05,])
  p <- plotSmear(cds, 
                 de.tags = de,
                 pair = c(ctl, trt))
}
