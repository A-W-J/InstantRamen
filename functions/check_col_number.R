check_col_number <- function(data){
  col_number <- row_number(data)/2
  if (col_number %% 1 == 0){
    out = list(col_number, col_number)
  }
  else {
    col_number_1 = col_number - 0.5
    col_number_2 = col_number + 0.5
    out = list(col_number_1, col_number_2)
  }
  out
}