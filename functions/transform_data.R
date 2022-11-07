transform_data <- function(data){
  data$hgnc_symbol <- convertIDs(rownames(top), "ENSEMBL", "SYMBOL", org.Hs.eg.db)
  data$entrezid <- convertIDs(rownames(top), "ENSEMBL", "ENTREZID", org.Hs.eg.db)
  data
}