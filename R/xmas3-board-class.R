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
#' @importFrom adana int2bin
#' @return matrix
#' @export
as.matrix.xmas3 <- function(x){
  nrow <- attr(x,"dims")[1]
  ncol <- attr(x,"dims")[2]
  l <- prod(attr(x,"dims"))
  my_data <- int2bin(x,l)
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
  n_rows <- attr(x,"dims")[1]
  n_cols <- attr(x,"dims")[2]
  my_data <- as.data.frame(x)
  my_data <- my_data -.5 #offset
  # y_adjust <- n_rows %/% 2
  # my_data$row <- ifelse(my_data$row)
  # offset <- 0.5
  # chessdat2 <- chessdat + offset
  g <- ggplot(my_data, aes(column,row)) + 
    geom_point(shape=22,size=20)
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
  class(g) <- c(class(g),"xmas3plot")
  g
}



#' @export
add_tile <- function(x,...){
  UseMethod("add_tile")  
}

#' Add Tile to Board Plot

#' @param col A-J or 1:10
#' @param row row in question
#' @param ... passed on
#' @param p 
#'
#' @export

add_tile.xmas3plot <- function(p, pos,...){
  .c <- substr(pos,1,1)
  .r <- substr(pos,2,2) %>% as.numeric()
  
  if(is.character(.c)){
    .c <- which(LETTERS==.c) 
  } 
  
  p + annotate("point",x=.c-.5,y=.r-.5,shape=22,size=20,...)
}
#' Add Move to Board Plot
#'
#' @param p Board Plot
#' @param move Move to Add
#' @param arr arrow to use
#' @param ... passed along (for color, linetype etc.)
#'
#' @return plot with move added
#' @export
add_move <- function (x, ...) {
  UseMethod("add_move")
}

#' @rdname add_move
#' @export
add_move.xmas3plot <- function(p,move,arr=arrow(),...){
  ## Parse move
  m <- parse_move(move)
  #browser()
  c1 <- m$cs[1];c2 <- m$cs[2]
  r1 <- m$rs[1]; r2 <- m$rs[2]
  ##offsets 
  if(c1==c2){
    c1<-c2-.5->c2
    if(r1>r2){
      r1<-r1-.60;
      r2<-r2-.40;        
    } else{
      r1<-r1-.4;
      r2<-r2-.6;        
    }

  }
  if(r1==r2){
    r1<-r2-.5->r2
    if(c1>c2){
      c1<-c1-.60;
      c2<-c2-.40;
    } else{
      c1<-c1-.40
      c2<-c2-.60
    }
    
  }
  
  p+ annotate("segment",x=c1,y=r1,xend=c2,yend=r2,arrow=arr,...)
  
}

#' Parse Move
#'
#' @param move Character vector representing the move, e.g. "B1 to B2" 
#'
#' @return a named list cell_specs c1,c2,r1,r2
#' @export
parse_move <- function(move){
  
  #move <- as.character(deparse(substitute(move))) ## why doesn't it work?
  # Get column
  cs <- stringr::str_extract_all(move,"[A-Z]", simplify = TRUE)
  cs <- c(which(LETTERS %in% cs[1]), which(LETTERS %in% cs[2])) 
  
  rs <- stringr::str_extract_all(move,"[0-9]{1,2}", simplify = TRUE) %>%
    as.integer()
  
  list(cs=cs,rs=rs)
}

if(interactive()){
  tmp <- xmas3board(1,dims = c(3,3))
  plot(tmp) -> p
  p<- p %>%
    add_move("B2 to C2") %>%
    add_move("C2 to C1") %>%
    add_tile("C2", color='gray') %>%
    add_tile("C1", color='gray') %>%
    add_move("C2 to C3") %>%
    add_move("C1 to B1")
  print(tmp)
  #print(p)
}
