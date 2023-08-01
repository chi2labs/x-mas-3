## Script check the coordinates and offset for robot
library(xmas3)
if(!exists("remDr")){
  robot_initialize()  
}

coords <- calculate_grid_centroids(surface_width = 760 ,surface_height = 480) #This is on linux
offset_x = 433
offset_y = 140
if(!exists("game_canvas")){
  game_canvas <<- remDr$findElements(using = "id","unity-canvas")  
}
j <-1 # for now
for(i in 1:10){
 # robot_make_move(1,i,"R")

}


for(i in 10:1){
#  robot_make_move(1,i,"L")
}

for(j in 2:6){
  for(i in 10:1){
#    robot_make_move(j,i,"L")
  }
  for(i in 1:10){
 #   robot_make_move(j,i,"R")
  }
}


for(i in 1:10){
  for(j in 1:6){
  robot_make_move(j,i,"D")
 }
readline("Continue?")
for(j in 6:1){
  robot_make_move(j,i,"U")
}
}


