#' Score the Board
#'
#' @param B the Board
#'
#' @return integer score
#' @export
score_binary_board <- function(B){
  
  # Magic numbers are 7,14,28, 30 and 31
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
             sum(totals%in%c(7,14,28))*50, #1110,0111, 11100 (and 0100)
             sum(totals%in%c(30))*100,
             sum(totals%in%c(30))*200
             )
  score
}