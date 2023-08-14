
#' Get ASCII from the Collection
#'
#' @param whichone 
#'
#' @return character vector with the art 
#' @export
#'
#' @examples
#' ascii_gatai()
ascii_gatai <- function(whichone=c("default","scared","surprised","tree")){
  w <- match.arg(whichone)
  my_art <- list(
    default = "

  /\\_/\\
 ( o.o )
  > ^ <

",
    tree = "
    /\\
   //\\\\
  ///\\\\\\
 ////\\\\\\\\
/////\\\\\\\\\\
   [][]
"
  )
  my_art[[w]]
}

