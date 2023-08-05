#' Agent 1
#'
#' Makes moves based on trained model
#' @param B Board
#' @import ReinforcementLearning
#' @return a list with the resulting board and metadata
#' @export
agent_1 <-\(B){
  if(!exists("model_1")){
    #Load model
    model_1 <<- readr::read_rds(system.file("models/model-1.rds",package = 'xmas3'))
    message("model_1.rds loaded")
  } 
  ret <- NULL
  for(tile in LETTERS[1:10]){
    BB <- binarize_board(B,tile)
    ## Scan for vertical moves ####
    W <- get_windows(BB,direction = "V")
    W$move <- predict(model_1,W$w)
    W <- W |> 
      dplyr::filter(move !="Pass")
    if(nrow(W)>0){
      #do first viable move
      my_move <- calculate_windowed_move(W$col[1],W$row[1],W$move[1],direction = "V")
      ret <- list(
        Board = move(B,my_move$row,my_move$col,my_move$move),
        metadata = data.frame(col=my_move$col,row=my_move$row,move=my_move$move,
                              direction="V",tile=tile, agent = "agent_1")
      
      )
      return(ret)
    }
  }
  
  ## Scan for horizontal moves ####
  for(tile in LETTERS[1:10]){
    BB <- binarize_board(B,tile)
    ## Scan for vertical moves ####
    W <- get_windows(BB,direction = "H")
    W$move <- predict(model_1,W$w)
    W <- W |> 
      dplyr::filter(move !="Pass")
    if(nrow(W)>0){
      #do first viable move
      my_move <- calculate_windowed_move(W$col[1],W$row[1],W$move[1],direction = "H")
      ret <- list(
        Board = move(B,my_move$row,my_move$col,my_move$move),
        metadata = data.frame(col=my_move$col,row=my_move$row,move=my_move$move,
                              direction="H",tile = tile, agent = "agent_1")
      )
      return(ret)
    }
  }
  if(is.null(ret)){
    ## If nothing is found make random move
    ret <- agent_0(B)
    ret$metadata$agent <- "delegated to agent_0" 
    ret$metadata$direction <- NA
    ret$metadata$tile <- NA
  }
  ret
}

if(interactive()){
  #B <- initialize_board()
  agent_1(BB3) |> print()
}
