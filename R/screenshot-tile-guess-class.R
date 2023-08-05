#' Guesses the Class of a Tile
#'
#' @param tile The tile to analyze (magick image)
#'
#' @return class name of tile
#' @export
screenshot_tile_guess_class <- function(tile){
  if(!exists("REFERENCE_TILES")){
    REFERENCE_TILES <<- readr::read_rds(
      system.file("models/reference_tiles.rds",  package = "xmas3")
    )
  }
  if(getOption("xmas3.switch_ref_names", FALSE)){
    names(REFERENCE_TILES) <- LETTERS[1:10]
  }
  purrr::map(REFERENCE_TILES, ~{
    sum(as.character(as.raster(tile))%in%.x)
  }) |> unlist() -> tmp
  names(tmp[which.max(tmp)])
}