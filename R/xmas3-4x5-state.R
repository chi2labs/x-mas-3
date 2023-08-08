#' Print State 4x5 State Space
#'
#' @param x integer
#' @param ... additional parameters to print function
#'
#' @return prints x as a binary matrix
#' @export
print.4x5.space <- function(x=0,...){
  #convert to to matrix and print
  as.matrix(x) |> 
    print()
  
}


as.matrix.4x5.space <- function(x=0,...){
  my_data <- intToBits(x) |> as.integer()
  M <- matrix(data = my_data[1:20],ncol = 4,nrow = 5)
  colnames(M) <- LETTERS[1:4]
  M
}