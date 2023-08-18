
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
                                   "isee",
                                   "beafraid",
                                   "bigtree","tree",
                                   "bigtreebroken",
                                   "bigtreedown","treedown","hiding")){
  w <- match.arg(whichone)
  my_art <- list(
    default = "

  /\\_/\\
 ( o.o )
  > ^ <

",
    isee = "

  /\\_/\\
 ( @.@ )
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
  _.-|   |           /\\_/\\   (`\\
 {   |   |          (o.o )__ _) )
  "-.|___|        _.> ^ <   `  /
  .--\'-`-.     _((_ ` --  /_<  \\
.+|______|__.-||__)`-\'(((/  (((/
',
    beafraid = '
 /\\___/\\
 \\ -.- /
 `-.^.-\'
   /"\\      
',
    bigtree = "
    *
    /\\
   //\\\\
  ///\\\\\\
 ////\\\\\\\\
/////\\\\\\\\\\
   [][]
",
    bigtreedown = "
   [][]
\\\\\\\\\\/////    
 \\\\\\\\////
  \\\\\\///
   \\\\//
    \\/
",
    tree = "
    /\\
   //\\\\
  ///\\\\\\
    []
",
    treedown = "
    []
  \\\\\\///
   \\\\//
    \\/
",
    bigtreebroken = "
    
    []
\\\\\\\\\\/////   /\\    
 \\\\\\\\////   //\\\\    *
"
  )
  my_art[[w]]
}

