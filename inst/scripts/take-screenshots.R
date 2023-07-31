library(wdman)
library(RSelenium)
selenium(retcommand = TRUE)
cDrv <- chrome()
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4567L,
  browserName = "chrome"
)

remDr$open()
remDr$maxWindowSize()
for(i in 11:30){
  remDr$navigate("https://gato-files-prod.s3.amazonaws.com/assets/games/X-MAS%203/index.html")
  # Wait for game to load
  Sys.sleep(10)
  game_canvas <- remDr$findElements(using = "id","unity-canvas")
  remDr$mouseMoveToLocation(x=800,y=250, game_canvas[[1]])
  remDr$click()
  Sys.sleep(10)
  remDr$screenshot(file = here::here("inst","image-data",paste0("screenshot-2023-07-31-1701-",i,".png")))
  cat("Iteration", i)
}
 