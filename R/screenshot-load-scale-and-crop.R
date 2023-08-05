#' Scale and Crop Screenshot
#' 
#' Returns an image with only the board on.
#'
#' @param img 
#' @param my_geometry 
#' @importFrom magick image_read image_resize image_crop geometry_area
#' @return a magick image
#' @export

screenshot_load_scale_and_crop <- function(img, my_geometry = geometry_area(710,426,250,134)){
  if(is.character(img))(
    img <- image_read(img)    
  )
  img |> 
    image_resize( "1200x700") |> 
    image_crop(my_geometry)
}