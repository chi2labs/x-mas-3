#' Agent 6
#' 
#' Plays a 5x3 grid strategy
#'
#' @param B Board Matrix
#' @param ... not in use for this agent
#' @import dplyr
#' @return a data.frame containing as least move_sequence 
#' @export
agent_6 <-function(B,...){
  my_moves <- evaluate_board_LUT_3x5(B) %>% 
    dplyr::filter(sequence !="Pass") %>% 
    group_by(Q) %>% 
    arrange(desc(expected_value)) %>% 
    #slice_head(n=1) %>% #one move sequence per Q
    ungroup() %>%
    arrange(Q)
  if(nrow(my_moves)==0){
    message("Couldn't find a playable move in the quadrants. Switching focus")
    for(i in 1:4){
      for(j in 1:6){
        win <- .get_matrix_window(B, from = c(i,j), dims = c(3,5))
        evaluate_3x5_grid(win) %>% 
          filter(sequence !="Pass")->tmp
        if(nrow(tmp) >0){
          message("Found Move with coordinates: ",LETTERS[j],i)
          #slide moves
          #browser()
          tmp$sequence <- purrr::map_chr(tmp$sequence,~{
            slide_move_sequence(.x,c(i,j))
          })
        }
        my_moves <- tmp
        break
      }
      if(nrow(my_moves)>0) break
    }
  }
  if(nrow(my_moves)==0){
    message("Found no playable moves anywhere. Making random move")
    A <- create_action_space(6,10)
    my_moves <- data.frame(
      Q = c(1),
      sequence = sample(A, size = 1),
      n_moves = 1
    )
  }
  my_moves
}
