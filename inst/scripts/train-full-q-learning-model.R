# Script to train the full model
library(xmas3)
library(ReinforcementLearning)
library(dplyr)
library(tibble)



n_row <- 6
n_col <- 3
control <- list(alpha = 0.3, gamma = 0.6, epsilon = 0.1)

filename_data <- here::here("inst","training-data",
                            paste0("xmas3-",n_row,"x",n_col,"-data.rds")
)
filename_model <- here::here("inst","models",
                             paste0("q-learning-",n_row,"x",n_col,".rds")
)
message("Filenames:\n", filename_data,"\n",filename_model)
# # 
# # # Set hyper parameters

S <- create_state_space(n_row,n_col)

A <- create_action_space(n_row,n_col)


## Remove Invalid State Spaces
my_scores <- purrr::map(S,score_binary_board) %>% unlist()
S <- S[my_scores==0] 
message("Removed Invalid States")

# Create data for initial state and actions ####
my_data <-  purrr::map(S ,~{
  tibble(State = list(.x),
         Action = A
  )
  
}) %>%  bind_rows()

message("Created first half of data")

## Play each move, each board

my_data2 <- purrr::map2(my_data$State, my_data$Action,~{
  B <- .x
  B2 <- make_move(B, .y)
  my_score <- score_binary_board( B2 )
  
  if(my_score == 0 & .y != "Pass"){
    my_score <- (-1) #Penalty
  }
  
  tibble(Reward = my_score, 
         NextState = list(B2) )
}) %>%  bind_rows()

message("played the games")

my_data <- bind_cols(my_data,my_data2)

readr::write_rds(my_data,file = filename_data)

message("Converting State and NextState to character for Q-L algorithm.")
my_data$State <- unlist(my_data$State) %>% as.character()
my_data$NextState <- unlist(my_data$NextState) %>% as.character()
message("Starting Training")
my_time <- system.time(
  
model <- ReinforcementLearning(
  data=my_data, s = "State", a = "Action", r = "Reward", s_new = "NextState", 
  control = control)
)

message("Saving model to: ", filename_model)

readr::write_rds(model,filename_model)

print(my_time)
