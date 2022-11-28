make_results <- function(cds, ctl, trt, ctl_n, trt_n){
  group <- c(rep(ctl, ctl_n), rep(trt, trt_n))
  #print(class(group))
  #print(group)
  design <- model.matrix(~group)
  #print(design)
  fit <- glmFit(cds, design)
  #print(fit)
  lrt <- glmLRT(fit)
  top <- topTags(lrt, n = nrow(cds$counts))$table
  top
}
