#' Makes random move
#'
#' @param B 
#'
#' @return a list with the resulting board and metadata
#' @export

agent_0 <-\(B){
   possible_moves <- c("U","D","L","R")
   cm <-c()
  .c <- sample(1:10,1)
  .r <- sample(1:6,1)
  # Make sure move is legal
  if(.c == 1 ){cm <- c(cm("U"))}
  if(.c == 10 ){cm <- c(cm("D"))}
  if(.r == 1)(cm <- c(cm("L")))
  if(.r == 10)(cm <- c(cm("R")))
  
  #Update Possible moves
  possible_moves <- possible_moves[!possible_moves%in%cm]
  
  my_move <- sample(possible_moves,1)
  
  
  list(
    Board = move(B,.r,.c,my_move),
    metadata = data.frame(col=.c,row=.r,move=my_move)
  )
  
}