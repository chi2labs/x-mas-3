#' Theme for X-mas-3 Boards
#' 
#' @param style 
#'
#' @return
#' @export
theme_xmas3 <- function(style = c("dark","light")){
  style <- match.arg(style)
  if(style=="dark"){
    theme <-
#      ggdark::dark_theme_bw()+
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(linetype = 2, color = 'grey'),
        axis.ticks = element_blank(),
        axis.text=element_text(size=20,face="bold", color="white"),
        axis.title = element_blank())

  } else {
    
    theme <-   theme_bw()+
      theme(
        panel.grid.major = element_blank(),
        # panel.grid.major = element_line(linetype = 2, color='gray'),
        panel.grid.minor = element_line(linetype = 2, color = 'grey'),
        axis.ticks = element_blank(),
        axis.text=element_text(size=20,face="bold"),
        #axis.text = element_blank(),
        axis.title = element_blank())  
    }
  theme
}



add_scales_xmas3 <- function(g){
  n_cols <- ncol(g$data)
  n_rows <- nrow(g$data)
  g+
   coord_cartesian(xlim=c(0,n_cols), ylim=c(n_rows,0))+
   scale_x_continuous(breaks = 1:n_cols-.5,labels=LETTERS[1:n_cols],expand = c(0,0))+
   scale_y_continuous(breaks = 1:n_rows-.5,labels = 1:n_rows, expand = c(0,0))
}

