process_data <- function(cds, Params){
  group <- c(rep(Params$control_name, Params$control_number), rep(Params$treatment_name, Params$treatment_number))
print(group)
print("checkpoint 2.1")
  design <- model.matrix(~group)
print("checkpoint 2.2")
  fit <- glmFit(cds, design)
print("checkpoint 2.3")
  lrt <- glmLRT(fit)
print("checkpoint 2.4")
  top <- topTags(lrt, n = nrow(cds$counts))$table
print("checkpoint 2.5")
  top
}
