# Script to Create a look-up-table for the model.
library(xmas3)
library(ReinforcementLearning)
library(dplyr)
library(usethis)



create_strategy_LUT <- function(n_row,n_col){
  
  filename_model <- here::here("inst","models",
                               paste0("q-learning-",n_row,"x",n_col,".rds")
  )
  LUT_name <- paste0("xmas3_LUT_",n_row,"x",n_col)
  message("Filenames:\n",filename_model)
  if(!file.exists(filename_model))stop("Cannot find model at: ",filename_model)
  
  S <- create_state_space(n_row,n_col)
  dims <- c(n_row, n_row)
  max_moves <- max(dims)
  ## Remove Invalid State Spaces
  my_scores <- purrr::map(S,score_binary_board) %>% unlist()
  S <- S[my_scores==0] 
  message("Removed Invalid States")
  
  model <-readr::read_rds(filename_model)
  
  message("Red model, Starting to play")
   my_data <- purrr::map(S,~{
    m <-0
    B <-xmas3board(.x, dims = c(n_row, n_col))
    # Check this here
    score <- 0
    move_seq <- c()
    while (score==0 & m <= max_moves) {
      m <- m+1
      my_move <- predict(model,B %>% as.character(.x))
      move_seq <-c(move_seq,my_move) 
      B <-make_move(B,my_move)
      score <- score_binary_board(B)
      if(my_move == "Pass") break
    }
    data.frame(State=.x,
               score =score, 
               expected_value = score/m, 
               n_moves = m, 
               sequence=paste0(move_seq, collapse = " -> ")
    )
  }) %>% bind_rows()
  
  assign(LUT_name,my_data,pos = 1)
  do.call("use_data", list(as.name(LUT_name), overwrite = TRUE))
}

## Uncomment as needed

 # create_strategy_LUT(n_row=2, n_col=5)
 # create_strategy_LUT(n_row=3, n_col=3)
 # create_strategy_LUT(n_row=3, n_col=4)
 # create_strategy_LUT(n_row=4, n_col=4)
 # create_strategy_LUT(n_row=3, n_col=5)
# create_strategy_LUT(n_row = 2, n_col=6)
# create_strategy_LUT( n_row = 6, n_col = 2 )
# create_strategy_LUT(n_row=5, n_col=3)
# create_strategy_LUT(n_row=3, n_col=6)
create_strategy_LUT(n_row=6, n_col=3)





