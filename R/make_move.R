#' Make move on binary Board
#'
#' @param B the board
#' @param move chess notation of the move i.e. "A1 to B1"
#'
#' @return the new board (a matrix)
#' @export
make_move <- function(x,...){
  UseMethod("make_move")
}

#' @rdname make_move
#' @export
make_move.xmas3 <- function(B, move){
  
  if(move == "Pass") return(B) #If no move is made the board remains unaltered
  # Parse out move
  M <- as.matrix(B)
  m <- parse_move(move)
  r1<-m$rs[1];r2<-m$rs[2];c1<-m$cs[1];c2<-m$cs[2]
  tmp <- M[r2,c2]
  M[r2, c2] <- M[r1,c1]
  M[r1, c1] <- tmp
  M %>%  adana::bin2int() %>%
    xmas3board(dims =attr(B, "dims") )
  
}


if(interactive()){
  tmp <- xmas3board(25,dims = c(3,3))
  # plot(tmp) -> p
  # p<- p %>% 
  #   add_move("B2 to C2") %>% 
  #   add_move("C2 to C1") %>% 
  #   add_tile("C2", color='gray') %>% 
  #   add_tile("C1", color='gray') %>% 
  #   add_move("C2 to C3") %>% 
  #   add_move("C1 to B1")
  print(tmp)
  tmp %>% make_move("B1B2") %>% print()
  print(tmp)
  tmp %>% make_move("B2A2") %>% print()
  
  #print(p)
}
