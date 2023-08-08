#' Creates State Space
#' 
#'
#' @param ncol width
#' @param nrow height
#'
#' @return an list of integers with class 4x5.space
#' @export
create_state_space <- function(ncol=4,nrow=5){
  classname <- paste0(ncol,"x",nrow,".space")
  S <- 1:2^(ncol*nrow)
  purrr::map(S,~{
    my_data <- intToBits(.x) |> as.integer()
    M <- matrix(data = my_data[1:20],ncol = 4,nrow = 5)
    colnames(M) <- LETTERS[1:4]
    M
    
  })
}
