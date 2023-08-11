
#' Instance of Robot
#' 
#' Handles the interaction with the web-driver.
#' @import R6
#' @name Robot
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
            initialize = function(
    conf_filename =  system.file("conf/chromebook.yml", package = "xmas3"),
    verbose = FALSE
            ){
              if(!file.exists(conf_filename))stop("Config file not found", conf_filename)
              self$config <- yaml::read_yaml(conf_filename)
              self$verbose <- verbose
              self$coords <- calculate_grid_centroids(self$config$board$width,self$config$board$height)
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
      # run: lsof -i :4567
      # and kill from command line
    },
    message = function(...){
      if(self$verbose)message(...)
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
    getGameCanvas = function(){
      self$remDr$findElements(using = "id","unity-canvas")
    },
      #' @field testRemoveDriver
      #' Makes every legal move on the board. Mostly for testing.
    testRemoteDriver = function(){
      A <- create_action_space(6,10)
      A <- A[which(A != "Pass")]
      purrr::walk(A,self$makeMove)
      
    },
    makeMoveSequence = function(moves){
      
    },
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
      remDr <- self$remDr
      game_canvas <- self$getGameCanvas()
      remDr$mouseMoveToLocation(x = conf$offset_x + orig$x ,
                                y = conf$offset_y + orig$y , game_canvas[[1]])
      remDr$click()
      
      remDr$mouseMoveToLocation(x = conf$offset_x + dest$x ,
                                y = conf$offset_y + dest$y , game_canvas[[1]])
      remDr$click()
      
    }
    # TODO refactor this to accept chess notation
          )
    
  )
if(interactive()){
  rob <- Robot$new(verbose=TRUE)
}
