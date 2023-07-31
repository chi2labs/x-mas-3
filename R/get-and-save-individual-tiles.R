#' Get Individual tiles
#' 
#' Chop the board up into individual tiles
#'
#' @param img the screenshot of the board 
#' @param coordinates calculated centroids
#' @param output_path where to save the files
#'
#' @return invisible()
#' @export
get_and_save_individual_tiles <- \(img,
                                   coordinates =null,
                                   output_path=here::here("inst","image-data","tiles"),
                                   filename="image.png"){

  date_time <- format(Sys.time(),"%Y-%B-%d-%H%M")  
  for(i in 1:nrow(coordinates)){
    crop_x <- coordinates$x[i] - 72
    crop_y <- coordinates$y[i] - 72
    cropped_img <- image_crop(img, geometry =paste("140x150+", crop_x,"+", crop_y))
    my_filename <- stringr::str_replace(filename,"\\.",paste0("-",date_time,"-",i,"\\."))
    image_write(cropped_img, paste0(path = output_path,"/",
                                    my_filename
                                    ))
  }
}

if(interactive()){
  system.time(
    get_and_save_individual_tiles(cropped_board,my_coords)
  ) |> print()
}
