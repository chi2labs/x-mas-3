#' Get Windows for a Matrix
#' 
#' Returns the windows for our matrix
#' Only works on 6x10 
#' @param M the matrix on which to operate
#' @param direction 
#' @param shape 1 by4 or 2 by 3 
#' @param size 3 (for now)
#' @importFrom dplyr bind_rows
#' @return a data frame with w (window), col(start colum), row(start row)
#' @export
get_windows <- function(M, direction = c("H","V"), shape = c("1x4","2x3"),size=3){
  if(missing(M))stop("Specify matrix")
  direction <- match.arg(direction)
  shape <- match.arg(shape)
  if(shape == "2x3") return(get_2_by_3_windows(M,direction))
  ret <- c()
  if(direction == "H"){
    ret <- purrr::map(1:(10-3),\(.c){
      purrr::map(1:6,\(.r){
        data.frame(w=
        M[.r,.c:(.c+3)] |> paste0(collapse = ""),
        col=.c,
        row=.r
        )
      }) |> bind_rows()
    }) |> bind_rows()
  }
  
  if(direction == "V"){
    ret <- purrr::map(1:(6-3),\(.r){
      purrr::map(1:10,\(.c){
        data.frame(w=
                     M[.r:(.r+3),.c] |> paste0(collapse = ""),
                   col=.c,
                   row=.r
        )
      }) |> bind_rows()
    }) |> bind_rows()
  }
  
  
  ret
}


get_2_by_3_windows <- function(M,direction){
  "Not implemented yet"
}
# M <- matrix(data = 1:60,6,10, byrow = TRUE)
# system.time(
#   get_windows(M,"V") |> print()
# )
