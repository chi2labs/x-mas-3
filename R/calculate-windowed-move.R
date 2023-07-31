#' Calculate Windowed Move
#' 
#' Convert a move from recommended for a specific window to one that is playable on the board.
#'
#' @param .col column
#' @param .row row
#' @param move Move to make
#' @param shape shape of the window
#' @param direction vertical or horizontal
#'
#' @return coordinates for the move function and move (UDLR)
#' @export
calculate_windowed_move <- \(.c,.r,move=c("First","Last","Middle"), shape = c("1x4","2x3"), 
                             direction=c("Horizontal","Vertical")){
  shape <- match.arg(shape)  
  move <- match.arg(move)
  direction <- match.arg(direction)
  my_row <-.r
  my_col <- .c
  if(direction == "Horizontal"& shape=="1x4"){
    my_row = .r
    my_col <- ifelse(move=="First",.c,.c+3)
    move <- ifelse(move == "First","R","L")
  }
  
  if(direction == "Vertical"& shape=="1x4"){
    my_col = .c
    my_row <- ifelse(move=="First",.r,.r+3)
    move <- ifelse(move == "First","D","U")
  }
  list(
    row = my_row,
    col = my_col,
    move = move
  )
}
