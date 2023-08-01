#' Return Column name With Highest Value
#'
#' @param data_frame 
#'
#' @return the column name
#' @export
find_column_with_highest_value <- function(data_frame) {
  
  highest_column_names <- vector("character", length = nrow(data_frame))
  
  
  for (i in 1:nrow(data_frame)) {
  
    row_data <- data_frame[i, ]
    
  
    highest_column_index <- which.max(row_data)
    
  
    highest_column_names[i] <- names(row_data)[highest_column_index]
  }
  
  highest_column_names
}
