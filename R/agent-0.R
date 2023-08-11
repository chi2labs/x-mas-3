#' Agent 0
#' 
#' Makes random moves
#'
#' @param S Screenshot of board
#' @param n number of moves to make
#' @param ... not in use for this agent
#'
#' @return a data.frame containing as least move_sequence 
#' @export

agent_0 <-function(S, n=100, ...){
  # We're playing random moves
  A <- create_action_space(6,10)
  A <- A[which(A!="Pass")]
  data.frame(sequence=sample(A,n,replace=TRUE))
}
