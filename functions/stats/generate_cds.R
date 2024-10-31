generate_cds <- function(data, Params){
  group <- c(rep(Params$control_name, Params$control_number), rep(Params$treatment_name, Params$treatment_number))
  counts = data
  cds <- DGEList( counts , group = group )
  cds <- calcNormFactors(cds, method = "upperquartile")
  scale <- cds$samples$lib.size*cds$samples$norm_factors
  normCounts <- round(t(t(counts)/scale)*mean(scale))
  cds <- estimateCommonDisp(cds, verbose = TRUE)
  cds <- estimateTagwiseDisp(cds)
  cds
}
