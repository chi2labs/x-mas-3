#' Generates Training Data for the Parallel State Space
#'
#' @return a data.frame with State Action Reward NextState
#' @importFrom dplyr mutate
#' @export

generate_parallel_training_data <- function(){
  my_states <- c("0000","0001","0010","0011","0100","0101","0110",
                 #"0111", #removes
                 "1000","1001","1010","1011","1100","1101"
                 #,"1110",#"1111" #removed
                 )
  actions <- c("Pass","First","Last")
  data.frame(
    State = rep(my_states,3),
    Action = rep(actions,13),
    Reward = -1,
    NextState = "0000"
  ) %>% 
    mutate(Reward = ifelse(Action == "Pass",1,Reward)) %>% 
    mutate(Reward=ifelse(State=="1101" & Action == "Last",1,Reward)) %>% 
    mutate(Reward=ifelse(State=="1101" & Action == "Pass",-1,Reward)) %>% 
    mutate(Reward=ifelse(State=="1011" & Action == "First",1,Reward)) %>% 
    mutate(Reward=ifelse(State=="1011" & Action == "Pass",-1,Reward))
    
  
}

 if(interactive()){
   generate_parallel_training_data() %>% print()
 }
