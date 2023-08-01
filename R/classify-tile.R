#' Classify Tile
#'
#' @param tile either a character vector (filename) or magick-image
#'
#' @return a data.frame with the prediction from each model
#' @importFrom dplyr tibble
#' @export
#' @examples
classify_tile <-function(tile){
  if(!exists("tile_classification_models")){
    tile_classification_models <<- readr::read_rds(
      system.file("models/tile-classification-models.rds",package="xmas3")
    )
  }
  purrr::map_df(tile,\(.x){
    
    # if(is.character(.x)){
    #   .x <- image_read(.x)
    # }
    my_color_analysis <- color_analysis(.x)
    
    purrr::map_dfc(names(tile_classification_models),\(tile_type){
      pred <- predict(
        tile_classification_models[[tile_type]],
        newdata = my_color_analysis,
        type=  "response"
      )
      ret <- tibble(pred)
      names(ret) <- tile_type
      ret
    })
  }) -> ret
  #names(ret) <- names(tile_classification_models)
  ret$prediction <- find_column_with_highest_value(ret)
  ret
}

if(interactive()){
  my_image <- c("inst/image-data/ui-tiles/image-2023-08-01 00_10_16-15.png",
                "inst/image-data/ui-tiles/image-2023-08-01 00_10_16-19.png")
  classify_tile(my_image)->test;test |> print()
}
