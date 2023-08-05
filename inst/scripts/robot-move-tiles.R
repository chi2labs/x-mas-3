## Script check the coordinates and offset for robot
library(xmas3)
if(!exists("remDr")){
  robot_initialize()  
}




for(i in 1:9){
  robot_make_move(1,i,"R")
  Sys.sleep(1)
}
for(i in 1:5){
  robot_make_move(1,"D")
  Sys.sleep(1)
}

for(i in 10:2){
  robot_make_move(1,i,"L")
}

for(j in 2:6){
  for(i in 10:2){
    robot_make_move(j,i,"L")
  }
  for(i in 1:9){
    robot_make_move(j,i,"R")
  }
}


for(i in 1:9){
  for(j in 1:6){
  robot_make_move(j,i,"D")
 }

for(j in 6:2){
  robot_make_move(j,i,"U")
}
}


