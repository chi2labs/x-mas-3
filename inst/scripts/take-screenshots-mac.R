# library(wdman)
# library(RSelenium)
# selenium(retcommand = TRUE)
# cDrv <- chrome()
# remDr <- remoteDriver(
#   remoteServerAddr = "localhost",
#   port = 4567L,
#   browserName = "chrome"
# )

remDr$open()
remDr$setWindowSize(width = 1200,height = 831)
for(i in 1:30){
  remDr$navigate("https://gato-files-prod.s3.amazonaws.com/assets/games/X-MAS%203/index.html")
  # Wait for game to load
  Sys.sleep(10)
  game_canvas <- remDr$findElements(using = "id","unity-canvas")
  remDr$mouseMoveToLocation(x=600,y=250, game_canvas[[1]])
  remDr$click()
  filename <- paste0("./inst/screenshots-mac/screenshot-2023-08-3-",i,".png")
  remDr$screenshot(file = filename)
  
  cat("Iteration", i)
}
 