#' Fecth tile from Screenshot
#'
#'
#' @param board the board weŕe playing on 
#' @param .c col
#' @param .r row
#' @param conf config
#' @param tile_width width
#' @param tile_height height
#' @importFrom magick  image_crop geometry_area image_write
#'
#' @return the tile in png format
#' @export
screenshot_get_tile <- function(board,.c, .r, 
                                conf, 
                                tile_width = 68, tile_height = 69){
  my_row <- conf$coords %>% 
    dplyr::filter(.data$col==.c,.data$row==.r)
  
  crop_x <- my_row$x - (tile_width/2)
  crop_y <- my_row$y - (tile_height/2)# - (conf$board_height/2)
  
  cropped_img <- 
    image_crop(
      board,
      #geometry = "100x100+10+10"
      geometry = 
        geometry_area(width = tile_width, height = tile_height, 
                      x_off = crop_x,
                      y_off = crop_y)
      #,
      #                         x_off = crop_x,y_off = crop_y)
    )  
  cropped_img
}
