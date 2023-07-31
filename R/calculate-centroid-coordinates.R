#' Get Centroids on Board
#' 
#' Calculates the centroids on the Board
#' 
#' @param surface_width width of board
#' @param surface_height height of board
#' @param num_rows number of rows
#' @param num_cols number of rows
#'
#' @return data.frame with col,row,x and y
#' @export
calculate_grid_centroids <- function(surface_width = 1450, surface_height = 890, num_rows=6, num_cols=10) {
  # Calculate the width and height of each grid cell
  grid_cell_width <- surface_width / num_cols
  grid_cell_height <- surface_height / num_rows
  
  # Initialize a list to store the centroid coordinates
  centroid_coordinates <- data.frame()
  # Loop through the rows and columns to calculate the centroid coordinates
  for (row in 1:num_rows) {
    for (col in 1:num_cols) {
      # Calculate the x-coordinate of the centroid
      centroid_x <- (col - 0.5) * grid_cell_width
      
      # Calculate the y-coordinate of the centroid
      centroid_y <- (row - 0.5) * grid_cell_height
      centroid_coordinates <- rbind(centroid_coordinates,data.frame(row=row,col=col,x=centroid_x,y=centroid_y))
    }
  }
  
  centroid_coordinates
}

if(interactive()){
  calculate_grid_centroids() |> print()
}
# # Define the dimensions of the surface and the grid
# surface_width <- 1450
# surface_height <- 890
# num_rows <- 6
# num_cols <- 10
# 
# # Calculate the centroid coordinates using the function
# centroid_coordinates <- calculate_grid_centroids(surface_width, surface_height, num_rows, num_cols)
# 
# # Print the centroid coordinates
# print(centroid_coordinates)
