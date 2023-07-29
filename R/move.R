#' Make a Move
#'
#' @param board the current (or inital) board
#' @param .r row
#' @param .c column
#' @param move Up/Down/Left/Right
#' 
#' @return The board after the move
#' @export
move <- function(board,.r=1,.c=1,move=c("U","D","L","R")){
  my_tile <- board[.r,.c]
  target_row <- switch (move,
                        "U" = .r-1,
                        "D" = .r+1,
                        .r
  )
  target_col <- switch (move,
                        "L" = .c-1,
                        "R" = .c+1,
                        .c
  )
  
  # If the move is illegal return board as is
  if(target_row == 0) return(board)
  if(target_row > dim(board)[1]) return(board)
  if(target_col==0) return(board)
  if(target_col > dim(board)[2]) return(board)
  
  board[.r,.c] <- board[target_row,target_col]
  board[target_row,target_col] <- my_tile
  board
}