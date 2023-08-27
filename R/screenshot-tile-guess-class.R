#' Guesses the Class of a Tile
#'
#' @param type type of prediction
#' @param tile The tile to analyze (magick image)
#'
#' @import nnet
#' @return class name of tile
#' @export
screenshot_tile_guess_class <- function(tile, type = "class"){
  if(!exists("GataiVision")){
    GataiVision <<- readr::read_rds(
      system.file("vision-model.rds",package = "xmas3")
    )
  }
  color_analysis(tile) %>% 
    predict(GataiVision, newdata=.,type=type) -> pred
  if(type=="class"){
    pred%>% 
    as.character()} else{
      pred
    }
  
}
