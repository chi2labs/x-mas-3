#' Agent 4
#' 
#' Plays a 4x4 Best Move Strategy
#'
#' @param B Board Matrix
#' @param ... not in use for this agent
#' @import dplyr
#' @return a data.frame containing as least move_sequence 
#' @export
agent_4 <-function(B, ...){
  suggested_moves <- play_strategy(B, "4x4")
  print(suggested_moves)
  suggested_moves |> 
    slice_head(n=1)
}


agent_5 <-function(B, ...){
  suggested_moves <- play_strategy(B, "6x3")
  print(suggested_moves)
  suggested_moves
}

# if(interactive()){
#   system.time({
#     
#   set.seed(1234)
#   B <- initialize_board()
#   agent_4(B) %>% print()
#   
#   }) |> print()
# }
