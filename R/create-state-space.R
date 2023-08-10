#' Creates State Space
#' 
#'
#' @param ncol width
#' @param nrow height
#'
#' @return an list of integers with class 4x5.space
#' @export
create_state_space <- function(nrow=3, ncol=3){
  classname <- paste0(nrow,"x",ncol,".space")
  S <- 0:(2^(ncol*nrow)-1)
  purrr::map(S,~{
   xmas3board(.x,dims = c(nrow,ncol))
  })
}
