#' Score the Board
#'
#' @param B the Board
#'
#' @return integer score
#' @export
score_binary_board <- function(B){
  if(!is.matrix(B)) B <- as.matrix(B)
  
  # Magic numbers here are:
  # 7,14,28 - three in a row/col
  # 15, 30 - four in a row/col
  # 31 - five in a row
  rows <- 
    apply(B,1,function(x){
      v <- paste0(x, collapse = "")
      strtoi(v,2)
    })
  
  cols <- 
    apply(B,2,function(x){
      v <- paste0(x, collapse = "")
      strtoi(v,2)
    })
  
  totals <- c(cols,rows)
  
  score <- sum(
             sum(totals%in%c(7,14,28))*50,
             sum(totals%in%c(15,30))*100, 
             sum(totals%in%c(31))*200
             )
  score
}