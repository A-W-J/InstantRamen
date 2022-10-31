preprocess_data <- function(data){
  data <- get_row_names(data)
  data <- remove_low_counts(data)
  data
}
