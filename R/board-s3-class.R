## S3 class to deal with Board
#' Get Board State
#'
#' @param x integer
#' @param dim dimensions of the grid
#'
#' @return x with class and attributes added
#' @export
xmas3board <- function(x=0, dims=c(4,5)){
  class(x) <- c("xmas3",class(x))
  attr(x,"dims") <- dims
  x
}

#' Converts Board State to Matrix
#'
#' @param x board
#'
#' @return matrix
#' @export
as.matrix.xmas3 <- function(x){
  my_data <- intToBits(x) |> as.integer()
  ncol <- attr(x,"dims")[1]
  nrow <- attr(x,"dims")[2]
  l <- prod(attr(x,"dims"))
  M <- matrix(data = my_data[1:l],ncol = ncol,nrow = nrow)
  colnames(M) <- LETTERS[1:ncol]
  M
}

#' Print Xmas3 Board
#'
#' @param x board
#' @param ... print parameters
#'
#' @return invisible NULL
#' @export

print.xmas3 <- function(x,...){
  print(as.matrix.xmas3(x))
}


as.data.frame.xmas3 <- function(x,...){
  df <- data.frame(row=integer(0))
  M <- as.matrix(x)
  for(.r in 1:nrow(M)){
    for(.c in 1:ncol(M)){
      if(M[.r,.c]==1){
        df <- df %>% 
          bind_rows(data.frame(column=.c, row=.r))
      }     
    }
  }
  df  
}

#' Plot Xmas3 Board
#'
#' @param x Board to Plot
#' @param ... 
#' @import ggplot2
#' @return a ggplot
#' @export
plot.xmas3 <- function(x,...){
  n_cols <- attr(x,"dims")[1]
  n_rows <- attr(x,"dims")[2]
  my_data <- as.data.frame(x)
  my_data <- my_data -.5 #offset
  # y_adjust <- n_rows %/% 2
  # my_data$row <- ifelse(my_data$row)
# offset <- 0.5
# chessdat2 <- chessdat + offset
  g <- ggplot(my_data, aes(column,row)) + geom_point(shape=22,size=20) + theme_bw()
  g <- g +
    theme_bw()+
     theme(
       panel.grid.major = element_blank(),
      # panel.grid.major = element_line(linetype = 2, color='gray'),
           panel.grid.minor = element_line(linetype = 2, color = 'grey'),
           axis.ticks = element_blank(),
           #axis.text = element_blank(),
           axis.title = element_blank())+
    coord_cartesian(xlim=c(0,n_cols), ylim=c(n_rows,0))+
    scale_x_continuous(breaks = 1:n_cols-.5,labels=LETTERS[1:n_cols],expand = c(0,0))+
    scale_y_continuous(breaks = 1:n_rows-.5,labels = 1:n_rows, expand = c(0,0))
  
  if(!is.null(list(...)$move)){ ## TODO: consider moving to separate function.
    ## Parse move
    move <- list(...)$move
    c1 <- stringr::str_sub(move,1,1)
    r1 <- stringr::str_sub(move,2,2) %>% as.integer()
    c2 <- stringr::str_sub(move,7,7)
    r2 <- stringr::str_sub(move,8,8) %>% as.integer()
    # Find numerical representations of column values
    c1 <- which(LETTERS == c1)
    c2 <- which(LETTERS == c2)
    ##offsets 
    c1<-c1-.5;r1<-r1-.5;c2<-c2-.5;r2<-r2-.5
    g <-g+ annotate("segment",x=c1,y=r1,xend=c2,yend=r2, arrow = arrow())
  }
  g
}

if(interactive()){
  tmp <- xmas3board(25)
  plot(tmp) %>% print()
}