#' Format Matrix or Array to Latex
#'
#' Helper function to generate latex tables from matrices or arrays.
#' @author Gregory Janesch
#' 
#' @param arr 
#'
#' @return Latex code
#' @export
array_to_LaTeX <- function(arr){
  rows <- apply(arr, MARGIN=1, paste, collapse = " & ")
  matrix_string <- paste(rows, collapse = " \\\\ ")
  paste("\\begin{bmatrix}", matrix_string, "\\end{bmatrix}")
}