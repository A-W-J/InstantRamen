transform_data <- function(data){
  data$hgnc_symbol <- convert_ids(
    rownames(data),
    "ENSEMBL",
    "SYMBOL",
    org.Hs.eg.db
  )
  data$entrezid <- convert_ids(
    rownames(data),
    "ENSEMBL",
    "ENTREZID",
    org.Hs.eg.db
  )
  data
}