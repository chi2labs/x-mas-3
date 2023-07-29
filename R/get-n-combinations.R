#' Number of Combinations
#' 
#' Returns the number of combinations on the board, i.e. 3 or more consecutive
#' simlar tiles in a row or column.
#'
#' @param m a matrix representing the board.
#'
#' @return the sum of the row and column matches
#' @export
get_n_combinations <- function(m){
  my_regex <- "([A-J])\\1\\1+"  
  rowmatch <- purrr::map_lgl(1:6,~{
    my_str <- stringr::str_c(m[.x,], collapse = "")
    grepl(my_regex,my_str) 
  })
  colmatch <-purrr::map_lgl(1:10,~{
    grepl(my_regex,stringr::str_c(m[,.x], collapse = "")) 
  })
  sum(colmatch,rowmatch)
}