get_row_names <- function(data){
  data = data[!duplicated(data[,1]),]
  data <- data.frame(data)
  rownames(data) = data[,1]
  data = data[,-1]
  data
}
