build_frame <- function(df1, df2, genes){
  lfc_1 <- c()
  lfc_2 <- c()
  for(gene in genes){
    gene_1 <- df1%>%
      filter(X == gene)
    val_1 <- gene_1$logFC
    lfc_1 <- c(lfc_1, val_1)
    gene_2 <- df2%>%
      filter(X == gene)
    val_2 <- gene_2$logFC
    lfc_2 <- c(lfc_2, val_2)
  }
  df <- data.frame(genes, lfc_1, lfc_2)
  df
}
