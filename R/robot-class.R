
#' Instance of Robot
#' 
#' Handles the interaction with the web-driver
#' @import R6
#' @name Robot
#' @export
library(R6) #Not sure why this is needed...
Robot <-
  R6Class("x-mas-3-robot",
          public = list(
            verbose = FALSE,
            config = NULL,
            remDr = NULL,
            cDrv = NULL,
            coords =NULL,
            game_canvas = NULL,
            initialize = function(){
              conf_filename <-  system.file("conf/chromebook.yml", package = "xmas3")
              if(!file.exists(conf_filename))stop("Config file not found", conf_filename)
              self$config <- yaml::read_yaml(conf_filename)
              
              self$coords <- calculate_grid_centroids(self$config$board$width,self$config$board$height)
              self$startRemoteDriver()
            },
            finalize = function(){
              self$message("Finalizing")
              tryCatch(self$remDr$close(),
                       error = function(e){
                         warning("Unable to close session","run: lsof -i :4567", " From a terminal to find the blocking process")
                       })
              tryCatch(self$cDrv$stop(),
                       error = function(e){
                         warning("Unable to close session","run: lsof -i :4567", " From a terminal to find the blocking process")
                       })
              # run: lsof -i :4567
              # and kill from command line
            },
            message = function(msg){
              if(self$verbose)message(msg)
            },
            startRemoteDriver = function(){
              self$message("Initializing x-mas-3 Robot")
              self$message("Starting Selenium")  
              selenium(retcommand = TRUE)
              #selenium()
              self$cDrv <- chrome()
              self$remDr <- remoteDriver(
                remoteServerAddr = "localhost",
                port = 4567L,
                browserName = "chrome"
              )
              self$message("Connecting to Chome Driver")
              self$remDr$open()
              self$remDr$setWindowSize(width = 1200,height = 831)
              self$message("Loading Game...")
              self$remDr$navigate("https://gato-files-prod.s3.amazonaws.com/assets/games/X-MAS%203/index.html")
              
              readline("Waiting for game do load. Press enter when it does.")
            },
            clickToGame = function(mode=c("arcade","time")){
              if(mode == "arcade"){
                self$message("Clicking through to Arcade Mode")
                self$game_canvas <- self$remDr$findElements(using = "id","unity-canvas")
                self$remDr$mouseMoveToLocation(x=800,y=250, self$game_canvas[[1]])
                self$remDr$click() 
              }
            },
            
            testRemoteDriver = function(){
              "Make every legal move on the board."
              self$message("Testing horizontal moves...")
              for(.c in 1:9){
                for(.r in 1:6){
                  self$makeMove(.c=.c,.r=.r,move="R")
                }
              }
              self$message("Testing vertical moves...")
              for(.r in 1:5){
                for(.c in 1:10){
                  self$makeMove(.r=.r,.c=.c,"D")
                }
              }
            },
            
            makeMove = function(.r,.c, move = c("U","D","L","R")){
              move <- match.arg(move)
              coords <- self$coords
              new_r <- .r 
              new_c <- .c
              if(self$verbose) self$message("Moving: ",.r,"x",.c, " ", move)
              
              if(move == "L"){ new_c <- .c-1}
              if(move == "R"){ new_c <- .c+1}
              if(move == "U"){ new_r <- .r-1}
              if(move == "D"){ new_r <- .r+1}
              
              orig <- dplyr::filter(self$coords,row==.r,col==.c)
              dest <- dplyr::filter(self$coords,row==new_r,col==new_c)
              #Checks
              if(sum(nrow(orig),nrow(dest))!=2){
                message("Move off the board detected")
                return(invisible())
              }
              conf <- self$config
              remDr <- self$remDr
              remDr$mouseMoveToLocation(x = conf$offset_x + orig$x ,
                                        y = conf$offset_y + orig$y , self$game_canvas[[1]])
              remDr$click()
              
              remDr$mouseMoveToLocation(x = conf$offset_x + dest$x ,
                                        y = conf$offset_y + dest$y , self$game_canvas[[1]])
              remDr$click()
            }
          )
          
  )

