#' Agent 5
#' 
#' Plays a 5x3 Quarter Strategy
#'
#' @param B Board Matrix
#' @param ... not in use for this agent
#' @import dplyr
#' @return a data.frame containing as least move_sequence 
#' @export
agent_5 <-function(B, ...){
  Q <- tibble(
    rows = c(1,4,1,4),
    cols = c(1,1,5,5)
  )
  moves <- purrr::map2(Q$rows,Q$cols,~{
    b <- get_sub_board(B,from = c(.x,.y), dims=c(3,5))
    evaluate_grid(b) %>% 
      mutate(Row = .x, Column = .y) %>% 
      filter(sequence !="Pass") 
    
  }) %>% bind_rows() 
   if(nrow(moves)>0){
  #   ## Slide moves
      moves$orig_seq <- moves$sequence
    for(i in 1:nrow(moves)){
      moves$sequence[i] <- slide_move_sequence(
        moves$sequence[i],
        pos = c(moves$Row[i], moves$Column[i])
        )
     # browser()
    }
   }
  return(moves)
}

if(interactive()){
  set.seed(1234)
  B <- initialize_board()
  agent_5(B) %>% print()

}
