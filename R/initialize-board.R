#' Initialize a game board
#'
#' @return a 6 by 10 Matrix with the letters [A-J] in the cells.
#' @export
initialize_board <- function(avoid_combinations = TRUE){
  tiles <- LETTERS[1:10]
  board <- matrix(data = sample(tiles,60,replace = TRUE),6,10)
  ## If the initial state scores, generate another one
  if(avoid_combinations && get_n_combinations(board)>0){
    board <- initialize_board()
  }
  class(board) <- c("xmas3full",class(board))
  board
}

#' @export
print.xmas3full<- function(B){
  M <- matrix(data = CIRCLED_TILES[as.character(B)],dim(B)[1],dim(B)[2])
  apply(M,1,function(.r){cat('|',.r,'|\n')})
}