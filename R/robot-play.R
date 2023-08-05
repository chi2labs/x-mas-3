#' Play a Game through the UI
#'
#' @param agent which agent to use
#'
#' @return invisible()
#' @export
robot_play_game <- function(agent=agent0, max_moves = 100){
  B <- initialize_board()
  for(i in 1:max_moves){
    ret <- agent_0(B)
    robot_make_move(ret$metadata$row,.c = ret$metadata$col,move = ret$metadata$move)
  }
}

#' Initialize the Robot
#' 
#' Starts the Robot, loads the game and presses play\
#' 
#' @import RSelenium
#' @import wdman
#' @return invisible()
#' @export
robot_initialize <- function(){
  message("Initializing x-mas-3 Robot")
  message("Starting Selenium")
  #selenium(retcommand = TRUE)
  #selenium()
  cDrv <- chrome()
  remDr <<- remoteDriver(
    remoteServerAddr = "localhost",
    port = 4567L,
    browserName = "chrome"
  )
  message("Connecting to Chome Driver")
  remDr$open()
  remDr$setWindowSize(width = 1200,height = 831)
  message("Loading Game...")
  remDr$navigate("https://gato-files-prod.s3.amazonaws.com/assets/games/X-MAS%203/index.html")
  
  readline("Waiting for game do load. Press enter when it does.")
  message("Clicking through to Arcade Mode")
  game_canvas <- remDr$findElements(using = "id","unity-canvas")
  remDr$mouseMoveToLocation(x=800,y=250, game_canvas[[1]])
  remDr$click()
  invisible()
}

#' Make a Move
#'
#' Makes a move on the canvas
#' 
#' @param .r row
#' @param .c Up Down Left Right
#' @param move move
#' @param offset_x canvas offset from board
#' @param offset_y canvas offset from board
#'
#' @return invisible()
#' @export
robot_make_move<- function(.r=1,.c=1,move=c("U","D","L","R")){
  if(!exists("game_canvas")){
    game_canvas <<- remDr$findElements(using = "id","unity-canvas")  
  }
   move <- match.arg(move)
   if(!exists("robot_conf")){
     robot_conf <<- robot_config_mac()
   }
   coords <- robot_conf$coords
  
  new_r <- .r 
  new_c <- .c
  message("Moving: ",.r,"x",.c, " ", move)
  # Calculate new position
  if(move == "L"){ new_c <- .c-1}
  if(move == "R"){ new_c <- .c+1}
  if(move == "U"){ new_r <- .r-1}
  if(move == "D"){ new_r <- .r+1}
  
  orig <- dplyr::filter(coords,row==.r,col==.c)
  dest <- dplyr::filter(coords,row==new_r,col==new_c)
  #Checks
  if(sum(nrow(orig),nrow(dest))!=2){
    message("Move off the board detected")
    return(invisible())
  }
  remDr$mouseMoveToLocation(x = robot_conf$offset_x+orig$x ,
                            y = robot_conf$offset_y+orig$y , game_canvas[[1]])
  remDr$click()
  
  remDr$mouseMoveToLocation(x = robot_conf$offset_x + dest$x ,
                            y = robot_conf$offset_y + dest$y , game_canvas[[1]])
  remDr$click()
  
  #Make Screenshot 
  # path_to_screenshot <-  tempfile(fileext = ".png")
  # remDr$screenshot(file = path_to_screenshot)
  # path_to_screenshot
}

robot_config_chroomebook<- function(){
  coords <- calculate_grid_centroids(surface_width = 760 ,surface_height = 480)
  offset_x <- 433
  offset_y <- 140
}

robot_config_mac <- function(){
  list(
    coords = calculate_grid_centroids(surface_width = 714 ,surface_height = 426),
    offset_x = 250,
    offset_y = 100,
    window = list(
      height=831,
      width = 1200
    )
  )
}
