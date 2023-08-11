#' Get Individual tiles
#' 
#' Chop the board up into individual tiles
#'
#' @param img the screenshot of the board 
#' @param coordinates calculated centroids
#' @param output_path where to save the files
#' @param filename output filename
#' @param size_x dimensions
#' @param size_y dimensions
#'
#' @return invisible()
#' @export
get_and_save_individual_tiles <- function(img,
                                   coordinates =null,
                                   output_path=here::here("inst","image-data","tiles"),
                                   filename="image.png",size_x=144,size_y=144){

  date_time <- format(Sys.time(),"%Y-%B-%d-%H%M")  
  for(i in 1:nrow(coordinates)){
    crop_x <- coordinates$x[i] - (size_x/2)
    crop_y <- coordinates$y[i] - (size_y/2)
    cropped_img <- image_crop(img, geometry =paste("140x150+", crop_x,"+", crop_y))
    my_filename <- stringr::str_replace(filename,"\\.",paste0("-",date_time,"-",i,"\\."))
    image_write(cropped_img, paste0(path = output_path,"/",
                                    my_filename
                                    ))
  }
}


