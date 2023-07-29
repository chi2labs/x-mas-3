#' Score and Update Board
#'
#' @param board 
#' @param with_score 
#'
#' @return list with updated board and score
#' @export

score_and_update_board <- function(board, with_score = TRUE){
  score <- 0
  get_scorings <- function(length_of_match){
    if(length_of_match == 3) return(50)
    if(length_of_match == 4) return(100)
    if(length_of_match == 5) return(200)
  }
  my_regex <- "([A-J])\\1\\1+"
  #row-wise check
  new_board <- board
  for(i in 1:6){
    my_string <- stringr::str_c(board[i,],collapse="")
    regex_res <- regexpr(my_regex, my_string)
    if(regex_res >0){
      expr_length <- attr(regex_res,"match.length")
      score <- score + get_scorings(expr_length)
      new_board[i,regex_res:(regex_res + expr_length-1)] <- NA
    }
  }
  for(i in 1:10){
    my_string <- stringr::str_c(board[,i],collapse="")
    
    regex_res <- regexpr(my_regex, my_string)
    
     if(regex_res >0){
      expr_length <- attr(regex_res,"match.length")
      score <- score + get_scorings(expr_length)
      new_board[regex_res:(regex_res + expr_length-1),i] <- NA
     }
  }
  
  list(
    board = new_board,
    score = score
  )
  
}




# if(interactive()){
#   set.seed(4361)
#   my_board <- initialize_board(avoid_combinations = FALSE) 
#   my_board |> print()
#   
#   new_board <- update_board(my_board) |> print()
#   
# }

if(interactive()){
  score_and_update_board(test_board)
}
