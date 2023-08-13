#' Evauate a Sub-Grid
#' 
#' Calculates move sequences for a grid, a subset of a board.
#'
#' @param G The Grid to evaluate
#'
#' @return
#' @export
evaluate_grid <- function(G){
  my_dims <- dim(G)
  tile_types <- unique(G %>% as.character() %>% sort())
  LUT <- get(paste0("xmas3_LUT_", dim(G)[1],'x',dim(G)[2]))
  res <- data.frame()
  for(tile in tile_types){
    State <-
      ifelse(tile==G,1,0) %>% 
      adana::bin2int() 
    res <- rbind(
      res, data.frame(Tile=tile,State)
    )
  }
  res %>% 
    left_join(LUT %>%  mutate(State=as.numeric(State)),by = c("State")) %>% 
    filter(sequence != "Pass")

}

#' Get a Subset of a Board
#'
#' @param B 
#' @param from coordinates of the top-left cell
#' @param dims dimensions
#'
#' @return The sub-matrix
#' @export
get_sub_board <- function(B,from,dims){
  B[from[1]:(from[1] + dims[1]-1),
    from[2]:(from[2] + dims[2]-1)
  ] -> B
  
  attr(B, "pos" )<-from
  B
}


# 
if(interactive()){
  set.seed(1234)
  B <- initialize_board()
  G <- get_sub_board(B, c(1,1) ,c(3,5) )
  evaluate_grid(G) %>% print()
}