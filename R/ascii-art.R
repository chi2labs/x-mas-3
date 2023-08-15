
#' Get ASCII from the Collection
#'
#' @param whichone 
#'
#' @return character vector with the art 
#' @export
#'
#' @examples
#' ascii_gatai()
ascii_gatai <- function(whichone=c("default","scared","surprised",
                                   "cool", "cash","computer",
                                   "bigtree","tree", "hiding")){
  w <- match.arg(whichone)
  my_art <- list(
    default = "

  /\\_/\\
 ( o.o )
  > ^ <

",
    cash = "

  /\\_/\\
 ( $.$ )
  > ^ <

",
    hiding = "
      |\\__/,|   (`\\
    _.|o o  |_   ) )
---(((---(((---------",
    
    cool = "
  /\\_/\\
 (⌐■_■ ) 
  > ^ <
",
    computer = '
      ___
  _.-|   |          |\\__/,|   (`\\
 {   |   |          |o o  |__ _) )
  "-.|___|        _.( T   )  `  /
  .--\'-`-.     _((_ `^--\' /_<  \\
.+|______|__.-||__)`-\'(((/  (((/
',
    bigtree = "
    /\\
   //\\\\
  ///\\\\\\
 ////\\\\\\\\
/////\\\\\\\\\\
   [][]
",
    tree = "
    /\\
   //\\\\
  ///\\\\\\
    []
"
  )
  my_art[[w]]
}

