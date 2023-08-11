# Script to Create a look-up-table for the model.
library(xmas3)
library(ReinforcementLearning)
library(dplyr)


n_row <- 3
n_col <- 5

filename_model <- here::here("inst","models",
                             paste0("q-learning-",n_row,"x",n_col,".rds")
)
message("Filenames:\n",filename_model)

S <- create_state_space(n_row,n_col)


## Remove Invalid State Spaces
my_scores <- purrr::map(S,score_binary_board) %>% unlist()
S <- S[my_scores==0] 
message("Removed Invalid States")

gatai <- readr::read_rds(filename_model)

s <- S[1:10]
max_moves <- 5
dims <- c(n_row, n_col)
model <- gatai

my_data <- purrr::map(S,~{
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
    if(my_move=="Pass") break
  }
  data.frame(State=.x,
             score =score, 
             expected_value = score/m, 
             n_moves = m, 
             sequence=paste0(move_seq, collapse = " ")
  )
}) %>% bind_rows()

readr::write_rds(my_data,file = "inst/LUTS/LUT-3x5.rds")
