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
  board
}