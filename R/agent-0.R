#' Makes random move
#'
#' @param B 
#'
#' @return a list with the resulting board and metadata
#' @export

agent_0 <-\(B){
  my_move <- sample(c("U","D","L","R"),1)
  .c <- sample(1:10,1)
  .r <- sample(1:6,1)
  list(
    Board = move(B,.r,.c,my_move),
    metadata = data.frame(col=.c,row=.r,move=my_move)
  )
  
}