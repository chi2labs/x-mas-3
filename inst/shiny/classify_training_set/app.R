#
# This app was created to facilitate the classification of image files for
# the computer vision algorithm
library(shiny)

my_files <- dir(here::here("inst","image-data","ui-tiles"),full.names = TRUE)

my_files <- sample(my_files,length(my_files))

file_path <- here::here("inst","training-data","image-classification.csv")

ui <- fluidPage(
  titlePanel("Classification"),
  sidebarLayout(
    sidebarPanel(
      actionButton(inputId = "candy", "Candy"),
      actionButton(inputId = "wreath","wreath"),
      actionButton(inputId = "hat", "hat"),
      actionButton(inputId = "berries","berries"),
      actionButton(inputId = "tree","tree"),
      
      actionButton(inputId = "striped","striped"),
      actionButton(inputId = "spotted","spotted"),
      actionButton(inputId = "stocking","stocking"),
      actionButton(inputId = "star","star"),
      actionButton(inputId = "red","red"),
      
      actionButton(inputId= "none","none")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("image")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  IDX <-1
  image_path <- reactiveVal(my_files[IDX])
  save_and_get_next <- function(what){
    
    file_conn <- file(file_path, "a")
    writeLines(paste0(image_path(),",",
                      what ,",", Sys.time()), 
               con = file_conn)
    close(file_conn)
    IDX <<-IDX +1
    image_path(my_files[IDX])
  }
  output$image <- renderPlot(
    {
      img <- image_read(image_path())
      plot(as.raster(img))
    }
  )
  observeEvent(input$spotted,{save_and_get_next("spotted")})
  observeEvent(input$candy,{save_and_get_next("candy")})
  observeEvent(input$hat,{save_and_get_next("hat")})
  observeEvent(input$berries,{save_and_get_next("berries")})
  observeEvent(input$tree,{save_and_get_next("tree")})
  
  observeEvent(input$wreath,{save_and_get_next("wreath")})
  observeEvent(input$stocking,{save_and_get_next("stocking")})
  observeEvent(input$star,{save_and_get_next("star")})
  observeEvent(input$red,{save_and_get_next("red")})
  observeEvent(input$striped,{save_and_get_next("striped")})
  
  observeEvent(input$none,{save_and_get_next("none")})
}

# Run the application 
shinyApp(ui = ui, server = server)
