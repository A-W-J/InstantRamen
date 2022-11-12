get_genes_list <- function(df, p_cutoff){
  df_filter <- df%>%
    filter(PValue <= p_cutoff)
  genes <- df_filter$X
  genes
}
