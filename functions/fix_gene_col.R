fix_gene_col <- function(df){
  df <- df%>%
    rename("gene" = ..1)
  df
}