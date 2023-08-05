#' Creates Matrix from Screenshot 
#'
#' @param B the screenshot
#'
#' @return a 6x10 Matrix
#' @export

screenshot_translate_board <- function(B, conf = robot_config_mac()){
  M <- matrix(nrow=6,ncol=10)
  
  purrr::walk(1:6,function(.r){
    purrr::walk(1:10, function(.c){
      my_tile <- screenshot_get_tile(board = B, .c, .r, conf)
      M[.r,.c] <<- screenshot_tile_guess_class(my_tile)
    })
  })
  M
}
