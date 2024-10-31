get_row_names <- function(data){
  data <- data.frame(data[!duplicated(data[,1]),])
  rownames(data) = data[,1]
  data = data[,-1]
  data
}
