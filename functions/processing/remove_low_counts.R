remove_low_counts <- function(data){
  means <- rowMeans(data)
  filter <- means >= 10
  table(filter)
  data <- data[filter,]
  data
}
