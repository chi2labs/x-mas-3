#' Convert binary string back to matrix
#'
#' @param x 
#'
#' @return matrix
#' @export
binary_to_matrix <- function(x){
  tmp <- stringr::str_split(x,"",simplify = TRUE) %>%  as.integer()
  M <- matrix(tmp, ncol = 4, nrow = 5)
  colnames(M) <- LETTERS[1:4]
  M
}
