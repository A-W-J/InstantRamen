make_barplot <- function(data){
  colors <- brewer.pal(9, "Set1")
  totCounts <- colSums(data)
  barplot(totCounts, las = 2, col = "blue")
}
