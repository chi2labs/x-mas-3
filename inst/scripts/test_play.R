# Testing
rm(list = ls())
library(xmas3)
library(RSelenium)
library(wdman)
#source("~/x-mas-3/R/robot-play.R")
robot_initialize()
robot_make_move(1,1,"R")
#robot_play_game()
options(xmas3.switch_ref_names=TRUE)
tmp_file <- tempfile(fileext = ".png")
for(i in 1:10){
    my_screenshot <- remDr$screenshot(file = tmp_file)
    Board <- xmas3::screenshot_load_scale_and_crop(tmp_file)
    
    M <- screenshot_translate_board(Board)
    P <- play_board(M,agent_1)
    mm <- P$metadata[1,]
    robot_make_move(.r = mm$row,.c=mm$col,move = mm$move)
}

#library(RSelenium)
#rD <- rsDriver(browser="firefox", port=4555L, verbose=F, chromever = NULL)
#rD$open()
