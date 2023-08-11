## Shoot-out agent 0 and agent 1
# Create 10000 boards
set.seed(5818)
library(xmas3)
library(dplyr)
my_boards <- purrr::map(1:10000L,function(x){initialize_board()})
system.time(
  my_data <- purrr::map_df(my_boards,function(B){
    play_board(B)$metadata
  })
)

# user  system elapsed 
# 10.323   0.016  10.369 

readr::write_rds(my_data,here::here("inst","evaluation-data","eval-agent-0-2023-07-31.rds"))

# Use agent 1
system.time(
  my_data2 <- purrr::map_df(my_boards,function(B){
    play_board(B,agent = agent_1)$metadata
  })
)

# user   system  elapsed 
# 1495.158    1.185 1499.686 
readr::write_rds(my_data2, here::here("inst","evaluation-data","eval-agent-1-2023-07-31.rds"))

