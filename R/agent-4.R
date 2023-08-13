#' Agent 4
#' 
#' Plays a 4x4 Grid 
#'
#' @param B Board Matrix
#' @param ... not in use for this agent
#' @import dplyr
#' @return a data.frame containing as least move_sequence 
#' @export
agent_4 <-function(B, ...){
  Q <- tibble(
    rows = c(1,3,1,3,2),
    cols = c(1,1,6,6,3)
  )
  
  moves <- purrr::map2(Q$rows,Q$cols,~{
    b <- get_sub_board(B,from = c(.x,.y), dims=c(4, 4))
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
    #browser()
    #moves$seq2 <- slide_move_sequence(moves$orig_seq,moves$Row,moves$Column)
    
  }
  if(nrow(moves)==0){
    message("Agent 4 found no playable moves, trying Agent 3")
    #moves <- agent_3(B)
    message("Agent 3 is on vacation maybe Agent 0 can help.")
    moves <- agent_0(B,n=1)
  }
  moves
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
