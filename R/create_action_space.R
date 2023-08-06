#' Create Action Space
#' 
#' Creates an action space for a board (grid of nrows by ncols)
#'
#' @param nrows board height
#' @param ncols board width
#'
#' @return a character vector with the possible moves
#' @export
create_action_space <- function(nrows,ncols){
  my_cols <- LETTERS[1:ncols]
  my_rows <- 1:nrows
  A <- c()
  # Vertical moves
  for(.r in 1:(length(my_rows))){
    for(.c in 1:(length(my_cols)-1)){
      A <- c(A,
             paste0(
               my_cols[.c],my_rows[.r],
               " to ",
               my_cols[.c+1],my_rows[.r]
             )
      )
    }
  }
  
  # Horizontal
  for(.c in 1:(length(my_cols)-1)){
    for(.r in 1:length(my_rows)){
      A <- c(A,
             paste0(
               my_cols[.c],my_rows[.r],
               " to ",
               my_cols[.c+1],my_rows[.r]
             )
      )
    }
    
  }
  
  c(A,"Pass")
}