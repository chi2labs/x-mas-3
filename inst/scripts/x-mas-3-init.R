library(wdman)
library(RSelenium)
#selenium(retcommand = TRUE)
cDrv <- chrome()
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4567L,
  browserName = "chrome"
)

remDr$open()
remDr$maxWindowSize()
remDr$navigate("https://gato-files-prod.s3.amazonaws.com/assets/games/X-MAS%203/index.html")
# Wait for game to load
Sys.sleep(7)
game_canvas <- remDr$findElements(using = "id","unity-canvas")
remDr$mouseMoveToLocation(x=800,y=250, game_canvas[[1]])
remDr$click()