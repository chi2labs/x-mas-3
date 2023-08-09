# Script to train the full model
library(xmas3)
library(ReinforcementLearning)
library(dplyr)

filename_data <- here::here("inst/training-data/xmas3-3-by-3-data.rds")
filename_model <- here::here("inst/models/q-learning-3x3.rds")

# Set hyper parameters
control <- list(alpha = 0.1, gamma = .1, epsilon = 0.1)

S <- create_action_space(3,3)
A <- create_action_space(3,3)


# Create data for initial state and actions
my_data <-  purrr::map(S ,~{
  data.frame(State = as.character(.x),
             Action = A
  )
  
}) |> bind_rows()

message("Created first half of data")


## Play each move, each board

my_data2 <- purrr::map2(my_data$State, my_data$Action,~{
  B <- .x
  B2 <- make_move(B,.y)
  my_score <- score_binary_board( B2 )
  if(my_score == 0 & .y != "Pass"){
    my_score <- (-1) #Penalty
  }

  data.frame(Reward = my_score, 
             NextState = B2 |> as.character() |> paste0(collapse=""))
  
}) |> bind_rows()

message("played the games")

my_data <- bind_cols(my_data,my_data2)

readr::write_rds(my_data,file = filename_data)



model <- ReinforcementLearning(
  data=my_data, s = "State", a = "Action", r = "Reward", s_new = "NextState", 
  control = control)

readr::write_rds(model,filename_model)
