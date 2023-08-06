#' Make a Move on a Specific board
#' 
#' Makes a move on a board. 
#'
#' @param Board a 6 by 10 Matrix with [A-J]
#' @param agent function which will make the moves. The default agent makes a random move on the board.
#'
#' @return a Matrix with the move made
#' @export
play_board <- function(Board,agent=agent_0){
  B <- agent(Board)
  my_score <- score_and_update_board(B$Board)$score[[1]]
  B$metadata$score <-my_score
  B
}

# if(interactive()){
#   B <- initialize_board()
#   play_board(B,agent_1) %>% print()
# }
