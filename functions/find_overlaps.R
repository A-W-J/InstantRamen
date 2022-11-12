find_overlaps <- function(df1, df2, size = 10, p_cutoff = 0.05){
  df1_genes <- get_genes_list(df = df1, p_cutoff)
  df2_genes <- get_genes_list(df = df2, p_cutoff)
  overlaps <- intersect(df1_genes, df2_genes)
  df1_cut <- df1%>%
    filter(X %in% overlaps)
  df2_cut <- df2%>%
    filter(X %in% overlaps)
  df1_sub <- head(df1_cut, size)
  genes <- df1_sub$X
  genes
}
