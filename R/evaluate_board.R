
.calculate_expected_values <-  function(model,S, dims=c(3,5)){
  max_moves <-max(dims) %>% unique()
  purrr::map(S,~{
       m <-0
       B <-xmas3board(.x,dims = dims)
       score <- 0
       move_seq <- ""
       while (score==0 & m <= max_moves) {
          m <- m+1
          my_move <- predict(model,B %>% as.character(.x))
          move_seq <-c(move_seq,my_move) 
          B <-make_move(B,my_move)
          score <- score_binary_board(B)
       }
         data.frame(score =score, 
                    expected_value = score/m, 
                    n_moves = m, 
                    sequence=paste0(move_seq, collapse = " ")
                    )
  }) %>% bind_rows()
  
  
  
  
  
  
  
}

.get_matrix_window<-function(M,from,dims){
  M[from[1]:(from[1] + dims[1]-1),
    from[2]:(from[2] + dims[2]-1)
  ] ->M
  
  attr(M,"pos")<-from
  M
}

get_matrix_quadrants <- function(M,dims){
  list(
    .get_matrix_window(M, c(1,1), dims),                    # Q1
    .get_matrix_window(M, c(1,
                            dim(M)[2]-dims[2]), dims), #Q2
    .get_matrix_window(M, c(dim(M)[1]-dims[1]+1,
                            1), dims), #Q3
    .get_matrix_window(M, c(dim(M)[1]-dims[1]+1,
                            dim(M)[2]-dims[2]+1), dims) #Q4
  )  
  
}

#' Evaluate a board based in a Model
#' 
#' The function will create sequential windows on the board, and pass these through the model.
#' 
#'
#' @param B The board
#' @param model the model to use
#' @param dims dimensions to use for windowing.
#' @param include_strategy should strategy be evaluated? (Slower)
#'
#' @importFrom adana bin2int
#' @return data.frame with suggested move and expected score
#' @export
evaluate_board <- function(B,model, dims=c(3,5),include_strategy = TRUE){
  W <- get_matrix_quadrants(B,dims)
  tile_types <- unique(B %>% as.character() %>% sort())
  res <- data.frame()
  Q=0
  for(w in W){
    Q <- Q+1
    pos <- attr(w,'pos')
    for(tile in tile_types){
      State <-
        ifelse(tile==w,1,0) %>% 
        bin2int()
      res <- rbind(
        res, data.frame(Q=Q,Row=pos[1],Column=pos[2],Tile=tile,State)
      )
      
    }
  }
  res$nextMove <- predict(model,res$State)
  #my_expected <- .calculate_expected_values(res$State,dims)
  res <- filter(res,(nextMove !="Pass"))
  if(include_strategy){
    expected_value <- .calculate_expected_values(model,res$State)
  res <-  cbind(res,expected_value)
  }
  res
}

if(interactive()){
  #library(dplyr)
  #library(xmas3)
  #library(adana)
  if(!exists("gatai")){
    gatai <- readr::read_rds("./inst/models/q-learning-3x5.rds")
  }
  
  set.seed(1234)
  B <- initialize_board(avoid_combinations = TRUE)
  system.time(
    E <- evaluate_board(B,gatai,c(3,5)) 
  ) %>% print()

  system.time(
    E <- evaluate_board(B,gatai,c(3,5),include_strategy = FALSE) 
  ) %>% print()  
}

