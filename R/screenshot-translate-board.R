#' Creates Matrix from Screenshot 
#'
#' @param B the screenshot
#' @param conf configuration 
#' @param create_training_pngs pngs for training the vision model be created 
#'
#' @import ReinforcementLearning
#' @return a 6x10 Matrix
#' @export

screenshot_translate_board <- function(B, conf = robot_config_mac(), create_training_pngs = FALSE){
  M <- matrix(nrow=6,ncol=10)
  
  purrr::walk(1:6,function(.r){
    purrr::walk(1:10, function(.c){
      my_tile <- screenshot_get_tile(board = B, .c, .r, conf)
      if(isTRUE(create_training_pngs)){
        my_filename <- here::here("training-tiles",paste(.c,"-",.r,".png"))
        image_write(my_tile,path = my_filename)
      }
      M[.r,.c] <<- screenshot_tile_guess_class(my_tile)
    })
  })
  M
}

