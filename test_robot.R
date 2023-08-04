library(RSelenium)
library(wdman)
selenium(retcommand = TRUE)
#selenium()
cDrv <- chrome()

remDr <<- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4567L,
  browserName = "Chrome"
)
message("Connecting to Chome Driver")
remDr$open()
#remDr$maxWindowSize()
#remDr$setWindowSize(1600,868)
remDr$getWindowSize()
message("Loading Game...")
remDr$navigate("https://gato-files-prod.s3.amazonaws.com/assets/games/X-MAS%203/index.html")
