#' Get All Sub-boards
#' 
#' Calculates and returns all possible sub-boards for a given geometry.
#'
#' @param B 
#' @param geometry 
#'
#' @return a list with the sub-boards
#' @export
get_all_sub_boards <- function(B, geometry="3x3"){
  dims <- parse_geometry(geometry)
  row_range <- 1:((dim(B)[1]-dims[1])+1)
  col_range <- 1:((dim(B)[2]-dims[2])+1)
  purrr::map(row_range,function(.r){
    purrr::map(col_range,function(.c){
      get_sub_board(B,c(.r,.c),dims)
    })
  })
}

#' Evaluate Sub-boards
#' 
#' Evaluates a list of sub-boards
#'
#' @param remove_duplicates should duplciate move sequences be removed
#' @param B the list of sub-boards
#'
#' @return a data.frame with recommended moves
#' @export
evaluate_sub_boards <- function(B, remove_duplicates=TRUE){
  ret <- purrr::map(B,~{
    purrr::map(.x,function(b){
      evaluate_grid(b)
    }) %>% bind_rows()
  }) %>% bind_rows()
  
  if(isTRUE(remove_duplicates)){
     ret <- ret[!duplicated(ret$sequence),]
   }
  ret
}

# if(interactive()){
#   set.seed(234)
#   B <- initialize_board()
#   get_all_sub_boards(B,"6x3") -> sb
#   ev1 <- evaluate_sub_boards(sb) |> print()
#   ev2 <- evaluate_sub_boards(sb, remove_duplicates =FALSE) |> print()
# }