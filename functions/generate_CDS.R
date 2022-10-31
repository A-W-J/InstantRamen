generate_CDS <- function(data, ctl, trt, ctl_n, trt_n){
  group <- c(rep(ctl, ctl_n), rep(trt, trt_n))
  data <- as.matrix(sapply(data, as.numeric))
  counts = data
  cds <- DGEList( counts , group = group )
  cds <- calcNormFactors(cds, method = "upperquartile")
  scale <- cds$samples$lib.size*cds$samples$norm_factors
  normCounts <- round(t(t(counts)/scale)*mean(scale))
  cds <- estimateCommonDisp(cds, verbose = TRUE)
  cds <- estimateTagwiseDisp(cds)
  cds
}
