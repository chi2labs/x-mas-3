# Speed-test
library(xmas3)

set.seed(2356)
score <- 0
system.time({

  for (i in 1:1000){
    board <- initialize_board()  
    row <- sample(1:6,1)
    col <- sample(1:10,1)
    .move <- sample(c("U","D","L","R"),1)
    board <- move(board,row,col,.move)
    score <- score + get_n_combinations(board)
  }
  print(score)
})

# user  system elapsed 
# 2.981   0.000   2.984 

set.seed(2356)
system.time({
  
  for (i in 1:1000){
    board <- initialize_board()  
    row <- sample(1:6,1)
    col <- sample(1:10,1)
    .move <- sample(c("U","D","L","R"),1)
    board <- move(board,row,col,.move)
   # score <- score + get_n_combinations(board)
  }
  #print(score)
})

# user  system elapsed 
# 2.280   0.008   2.294 




set.seed(2356)
system.time({
  score <- 0  
  for (i in 1:1000){
    board <- initialize_board()  
    row <- sample(1:6,1)
    col <- sample(1:10,1)
    .move <- sample(c("U","D","L","R"),1)
    board <- move(board,row,col,.move)
    s_u <- score_and_update_board(board)
    score <- score + s_u$score
  }
  print(score)
})
# [1] 2250
# user  system elapsed 
# 2.835   0.000   2.847
