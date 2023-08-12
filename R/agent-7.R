#' Agent 7
#' 
#' Plays a 5x3 grid strategy
#'
#' @param B Board Matrix
#' @param ... not in use for this agent
#' @import dplyr
#' @return a data.frame containing as least move_sequence 
#' @export
agent_7 <-function(B,...){
  my_moves <- evaluate_board_LUT_3x5(B) %>% 
    dplyr::filter(sequence !="Pass") %>% 
    group_by(Q) %>% 
    arrange(desc(expected_value)) %>% 
    #slice_head(n=1) %>% #one move sequence per Q
    ungroup() %>%
    arrange(Q)
  
  if(nrow(my_moves)==0){
    message("Found no playable moves. Making random move")
    A <- create_action_space(6,10)
    my_moves <- data.frame(
      Q = c(1),
      sequence = sample(A, size = 1),
      n_moves = 1
    ) 
  }
  my_moves
}
