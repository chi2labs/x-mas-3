#' Make Board Binary
#' 
#' Turns the board matrix into a binary matrix based in the input letter
#'
#' @param Board 
#' @param tile 
#'
#' @return a character matrix with "0" and "1"
#' @export
binarize_board <-\(Board, tile = LETTERS[1:10]){
  tile <- match.arg(tile)
  apply(Board,2,\(x){ifelse(x==tile,1,0)})
}

# if(interactive()){
#   B <- initialize_board()
#   print(B)
#   binarize_board(B,"A") |> print()
#   
# }