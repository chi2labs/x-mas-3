#' Play a Board with a Strategy
#'
#' @param B the board
#' @param geometry geometry of the strategy
#' @param strategy which one to choose (choose best move)
#'
#' @return move sequence(s) selected and expected score per move
#' @export
play_strategy <- function(B, geometry="3x3", strategy=c("bestmove")){
  BS <- get_all_sub_boards(B,geometry)
  res <- evaluate_sub_boards(BS)
  if(strategy == "bestmove"){
    res <- res %>%
      arrange(desc(expected_value), n_moves)
  }
}

# if(interactive()){
#   system.time(
#     {
#       set.seed(123)
#       for (i in 1:10){
#         
#         B <- initialize_board()
#         moves <- play_strategy(B, "6x3")
#       }
#       
#       
#       
#     }
#     
#   ) |> print()
#   
# }