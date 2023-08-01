#' Analyze Color in an Image
#' 
#' Analyzes the colors of an image and returns overall mean along with means for the quadrants.
#' Reduces the color space to RBG.
#'
#' @param img either a "magick-image or character-vector (filename) 
#' @importFrom purrr map_df
#' @importFrom magick image_read
#' @importFrom stringr str_sub
#' @return a data.frame with variables
#' @export 
color_analysis <- \(img){
  
  purrr::map_df(c(img),\(.x){
    
    if(is.character(.x)){
      .x <- image_read(.x)
    }
    tmp <- as.raster(.x) |> as.character()
    tmp <- str_sub(tmp,1,7) |> 
      col2rgb()
    l <- length(tmp)/3
    l <- seq(l/4,l,by=l/4)
    
    data.frame(
      R   = mean(tmp[1,]),
      G   = mean(tmp[2,]),
      B   = mean(tmp[3,]),
      #Q1
      Q1R = mean(tmp[1,1:l[1]]), 
      Q1G = mean(tmp[2,1:l[1]]), 
      Q1B = mean(tmp[3,1:l[1]]), 
      
      Q2R = mean(tmp[1,l[1]:l[2]]), 
      Q2G = mean(tmp[2,l[1]:l[2]]), 
      Q2B = mean(tmp[3,l[1]:l[2]]),
      
      Q3R = mean(tmp[1,l[2]:l[3]]), 
      Q3G = mean(tmp[2,l[2]:l[3]]), 
      Q3B = mean(tmp[3,l[2]:l[3]]), 
      
      Q4R = mean(tmp[1,l[3]:l[4]]), 
      Q4G = mean(tmp[2,l[3]:l[4]]), 
      Q4B = mean(tmp[3,l[3]:l[4]]) 
      
    )
  })
}