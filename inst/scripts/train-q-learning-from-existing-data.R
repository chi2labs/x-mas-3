# Script to train the full model
library(xmas3)
library(ReinforcementLearning)
library(dplyr)
library(tibble)



n_row <- 3
n_col <- 6
control <- list(alpha = 0.3, gamma = 0.6, epsilon = 0.1)

filename_data <- here::here("inst","training-data",
                            paste0("xmas3-",n_row,"x",n_col,"-data.rds")
)
filename_model <- here::here("inst","models",
                             paste0("q-learning-",n_row,"x",n_col,".rds")
)
message("Filenames:\n", filename_data,"\n",filename_model)
# # 


my_data <- readr::read_rds(filename_data)


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