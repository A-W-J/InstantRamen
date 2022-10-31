make_results <- function(data, cds, ctl, trt, ctl_n, trt_n){
  group <- c(rep(ctl, ctl_n), rep(trt, trt_n))
  design <- model.matrix(~group)
  fit <- glmFit(cds, design)
  lrt <- glmLRT(fit)
  top <- topTags(lrt, n = nrow(cds$counts))$table
  genes <- row.names(data)
  top <- cbind(genes = genes, top)
  top
}
