#' Make move on binary Board
#'
#' @param B the board
#' @param move chess notation of the move i.e. "A1 to B1"
#'
#' @return the new board (a matrix)
#' @export
make_move <- function(B, move){
  if(move == "Pass") return(B) #If no move is made the board remains unaltered
  # Parse out move
  # Has format: "A1 to B1"
  c1 <- stringr::str_sub(move,1,1)
  r1 <- stringr::str_sub(move,2,2) %>% as.integer()
  c2 <- stringr::str_sub(move,7,7)
  r2 <- stringr::str_sub(move,8,8) %>% as.integer()
  
  ## Move will be in the format A1A2
  tmp <- B[r2,c2]
  B[r2, c2] <- B[r1,c1]
  B[r1, c1] <- tmp
  B
}