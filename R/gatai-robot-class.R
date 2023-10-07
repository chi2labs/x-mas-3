
#' Instance of Robot
#' 
#' Handles the interaction with the web-driver.
#' @import R6
#' @name GatAIRobot
#' 
#' @import R6
#' 
#' @examples
#' \dontrun{
#' ## Play every legal move on the board.
#'gatai <- Robot$new()
#'gatai$clickToGame()
#'gatai$testRemoteDriver()
#' }
#' 
#' @export
GatAIRobot <-
  R6Class("x-mas-3-robot",
          public = list(
            verbose = FALSE,
            config = NULL,
            remDr = NULL,
            cDrv = NULL,
            coords =NULL,
            screenshotFile = character(0),
            board_geometry= NULL,
            huddleRate = 10L, # Number of seconds
            initialize = function(
    #conf_filename =  system.file("conf/chromebook.yml", package = "xmas3"),
              conf_filename =  system.file("conf/mac-mini.yml", package = "xmas3"),
              verbose = FALSE
              
            ){
              self$screenshotFile <- tempfile(fileext = ".png")
              if(!file.exists(conf_filename))stop("Config file not found", conf_filename)
              self$config <- yaml::read_yaml(conf_filename)
              self$verbose <- verbose
              self$coords <- calculate_grid_centroids(surface_width = self$config$board$width,
                                                      surface_height = self$config$board$height)
              self$config$coords <- self$coords
              b <- self$config$board
              self$board_geometry <- geometry_area(b$width,
                                                   b$height,
                                                   self$config$offset_x,
                                                   self$config$offset_y)
              self$startRemoteDriver()
            },
    finalize = function(){
      self$message("Finalizing xmas3 robot.")
      tryCatch(self$remDr$close(),
               error = function(e){
                 warning("Unable to close session","run: lsof -i :4567", " From a terminal to find the blocking process")
               })
      tryCatch(self$cDrv$stop(),
               error = function(e){
                 warning("Unable to close session","run: lsof -i :4567", " From a terminal to find the blocking process")
               })
      # sudo kill -9 $(sudo lsof -t -i:4567)
      # run: lsof -i :4567
      # and kill from command line
    },
    message = function(...){
      if(self$verbose)message(...)
    },
    startRemoteDriver = function(){
      self$message("Initializing x-mas-3 Robot")
      self$message("Starting Selenium")  
      ## For mac
      my_command <- "/usr/bin/java -Dwebdriver.chrome.driver='/Users/sasha/chromedriver' -Dwebdriver.gecko.driver='/Users/sasha/Library/Application Support/binman_geckodriver/macos/0.33.0/geckodriver' -Dphantomjs.binary.path='/Users/sasha/Library/Application Support/binman_phantomjs/macosx/2.1.1/phantomjs-2.1.1-macosx/bin/phantomjs' -jar '/Users/sasha/Library/Application Support/binman_seleniumserver/generic/4.0.0-alpha-2/selenium-server-standalone-4.0.0-alpha-2.jar' -port 4567"
      # 
      system(my_command, wait = FALSE)
      Sys.sleep(1)
      
      
      # selenium(retcommand = TRUE)
      # #selenium()
      # self$cDrv <- chrome()
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
    
    #' @field takeScreenshot
    #' Takes a screenshot of the current screen
    takeScreenshot = function(){
      self$remDr$screenshot(file = self$screenshotFile)
    },
    
    
    #' @field getBoardOnScreen
    #'  description
    #' Takes a screenshot of the current screen, crops it and
    #' returns the cropped board.
    getBoardOnScreen = function(){
      self$takeScreenshot()
      Board <- screenshot_load_scale_and_crop(
        self$screenshotFile,
        my_geometry = self$board_geometry
      )
      
      B <- screenshot_translate_board(Board, conf = self$config)
      # Do check here if board is empty of "done" ####
      B
    },
    
    #' @field play
    #' Plays the current board using one of the GatAI agents.
    #' @param agent a GatAi Agent function.
    #' @param n_moves How many moves to play
    #' @param record Should a game record be produced?
    #' @param record show Should the moved be shown on screen before they are played.
    play = function(agent = agent_0, n_moves = 100, record = FALSE, 
                    show=FALSE){
      
      record_file <- record
      if(!isFALSE(record)){
        if(!is.character(record)) record_file <- tempfile(fileext = ".rds")
        file.create(record_file)
      }
      record <- list()
      
      max_moves <- n_moves
      move_n <- 0
      while(move_n < max_moves){
        move_n <- move_n + 1
        self$message("Move number: ", move_n)
        S <- self$getBoardOnScreen()
        moves <- agent(S)
        record <- c(record, moves)
        #move_n <- move_n + sum(moves$n_moves) # update Move count
        purrr::walk(moves$sequence, ~{
          if(isTRUE(show)) self$showMoveOnCanvas(.x, delay =1000)
          self$makeMoveSequence(.x)
          }
          )  
      }
      if(!isFALSE(record_file)){
        message("Saving game record to: ",record_file)
        readr::write_rds(record, file=record_file)
        
      }
    },
    
    #' @field monitorGame
    #' Monitors the play on the board and intervenes if there is a huddle.
    monitorGame = function(huddleRate = 5, sentinel = 5){
      self$huddleRate <- huddleRate
      
      # Get reference board
      self$takeScreenshot()
      last_screen <- image_read(self$screenshotFile) %>% 
        as.raster()
      Sys.sleep(self$huddleRate)
      
      # Start monitoring
      idx <- 1
      while (idx < sentinel) {
        idx <- idx + 1
        self$message("Monitor: ",idx)
        # Check if reference is equal to new screenshot
        self$takeScreenshot()
        current_screen <- image_read(self$screenshotFile) %>% 
          as.raster() 
        my_cond <- all.equal(last_screen,current_screen)
        
        if(isTRUE(my_cond)) {
          message("Huddle detected.")
          self$showBestMove()
        } else{
          last_screen <- current_screen
        }
        Sys.sleep(self$huddleRate) 
      }
      
    },
    
    #' @field messgeToPlayer
    #' Sends a message via javascript to the player
    messageToPlayer = function(msg){
      self$remDr$executeScript(paste0("alert('",msg,"');"))
    },
    #' @field showMoveOnCanvas
    #' Shows a move on the canvas
    showMoveOnCanvas = function(move, delay = 1000){
      purrr::walk(move,\(move){
        Sys.sleep(1) #kunstpause
      # Get coordinates
      m <- parse_move(move)
      orig <- dplyr::filter(self$coords,row==m$rs[1], col==m$cs[1])
      dest <- dplyr::filter(self$coords,row==m$rs[2], col==m$cs[2])
      x <- self$config$offset_x + orig$x
      y <- self$config$offset_y + orig$y
      
      # Horizontal or Vertical ?
      if(m$rs[1]==m$rs[2]){ #same row
        my_arrow <-   "⇒"
        #x <-  x + 36 
        #adjust for tile size
        y <- y -36
      } else {
        my_arrow <- "⇓"
        x <- x -36
      }
      
      
      # arrows <- list(
      #   down = "⇓",
      #   up = "⇑",
      #   left = "⇒",
      #   right = "⇐"
      # )
      
      my_con <- file(here::here("js","insert-div.js"))
      js <- readLines(my_con)
      js <- paste0(js,collapse = "")
      close(my_con)
      
      js2 <- glue::glue('createArrowOverlay({top}, {left}, {width}, {height}, {delay}, "{arrow}",80);',
                        top=y,left=x, width = 72, height = 72, delay = delay, arrow = my_arrow );  
      js <- paste0(js,js2,collapse = ";")
      self$remDr$executeScript(js)
      
      })
      
    },
    
    #' @field showMoveOnBBestMove
    #' Shows the higest scoring move to the player for the current board.
    showBestMove = function(){
      B <- self$getBoardOnScreen()
      suggested_moves <- play_strategy(B, "4x4") %>% 
        dplyr::filter(score>0) %>% 
        dplyr::arrange(n_moves) %>% 
        head(4) %>% 
        dplyr::pull("sequence") %>% 
        unlist()-> tmp
       # Show Gatai Assist.
      self$showGataiAssist(moves = tmp)
      Sys.sleep(2) # Kunstpause
      self$showMoveOnCanvas(tmp)
      
    },
    
    
    #' @field showGataiAssist
    #' Displays a div with Gatai Assist on the game canvas
    showGataiAssist = function(moves = c("A1B1"), chitchat = "Having trouble?\\n\\nHere are some moves you might try:\\n\\n"){
      
      my_con <- file(here::here("js","insert-div.js"))
      js <- readLines(my_con)
      js <- paste0(js,collapse = "")
      close(my_con)
      
      text1 <- glue::glue("\\n\\n '/\\\\_/\\\\ \\n( o.o )\\n'> ^ <\\n\\n\\n")
      
      text2 <- paste0(chitchat,
                      paste0(moves, collapse="\\n"), 
                      collapse="\\n")
      
      
      js2 <- glue::glue('
              insertTeletypeDiv("{text1}", "{text2}", 0, 0);
              ')
      
      js <- paste0(js,js2,collapse = ";")
      self$remDr$executeScript(js)
    },
    
    
    #' @field clickToGame
    #' Clicks into the game from the welcome screen.
    clickToGame = function(mode=c("arcade","time")){
      if(mode == "arcade"){
        self$message("Clicking through to Arcade Mode")
        self$game_canvas <- self$remDr$findElements(using = "id","unity-canvas")
        #self$remDr$mouseMoveToLocation(x=800,y=250, self$game_canvas[[1]])
        
        self$remDr$mouseMoveToLocation(x = self$config$arcade$x_cord,
                                       y = self$config$arcade$y_cord, self$game_canvas[[1]])
        self$remDr$click() 
      }
    },
    
    #' @field getGameCanvas
    #' Returns the binding to the game canvas.
    getGameCanvas = function(){
      self$remDr$findElements(using = "id","unity-container")
    },
    
    #' @field testRemoveDriver
    #' Makes every legal move on the board. Convenience function for testing purposes.
    testRemoteDriver = function(){
      A <- create_action_space(6,10)
      A <- A[which(A != "Pass")]
      purrr::walk(A,self$makeMove)
      
    },
    
    makeMoveSequence = function(moves){
      m <- parse_move_sequence(moves)
      purrr:::walk(m,self$makeMove)
    },
    
    #' @field makeMove
    #' Makes a move on the screen
    makeMove = function(move){
      #Parse move etc.
      m <- parse_move(move)
      self$message("Moving: ", move)
      orig <- dplyr::filter(self$coords,row==m$rs[1], col==m$cs[1])
      dest <- dplyr::filter(self$coords,row==m$rs[2], col==m$cs[2])
      if(sum(nrow(orig),nrow(dest))!=2){
        self$message("Move off the board detected")
        return(invisible())
      }
      conf <- self$config
      # return(invisible()) ## debugging ####
      remDr <- self$remDr
      
      orig$x <- orig$x-(conf$canvas$width/2)
      orig$y <- orig$y-(conf$canvas$height/2)
      dest$x <- dest$x-(conf$canvas$width/2)
      dest$y <- dest$y-(conf$canvas$height/2)
      
      game_canvas <- self$getGameCanvas()
      remDr$mouseMoveToLocation(x = conf$offset_x + orig$x ,
                                y = conf$offset_y + orig$y ,
                                game_canvas[[1]])
      remDr$click()
      
      remDr$mouseMoveToLocation(x = conf$offset_x + dest$x ,
                                y = conf$offset_y + dest$y ,
                                game_canvas[[1]])
      remDr$click()
      
    }
          )
    
  )

# Debugging ####
if(interactive()){
  library(R6)
  library(magick)
  library(RSelenium)
  library(wdman)
  library(xmas3)
  library(magick)
  if(exists("gat")){
    rm(gat)
    gc()
    
  }
  gat <- GatAIRobot$new(verbose=TRUE
  ,conf_filename = "./inst/conf/mac-mini.yml")
  gat$takeScreenshot()
  Board <- screenshot_load_scale_and_crop(
    gat$screenshotFile,
    my_geometry = gat$board_geometry
  )
  
  print(Board)
  # gat <- GatAIRobot$new(verbose=TRUE,conf_filename = "./inst/conf/chromebook.yml")
  
  # gat$play(agent_6)
  
}
