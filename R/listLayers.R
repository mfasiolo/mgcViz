#'
#' Lists available layers for plotSmooth objects
#'
#' @description This function takes as input an object of class \code{plotSmooth} and
#'              returns the names of all the possible visual layers that
#'              could be used with that object.
#'
#' @param o an object of class \code{plotSmooth}.
#' @return A vector containing the names of the available layers.
#' @examples 
#' library(mgcViz)
#' n  <- 400
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#' dat <- data.frame("x1" = x1, "x2" = x2,
#'                   "y" = sin(x1) + 0.5 * x2^2 + rnorm(n))
#' b <- gam(y ~ x1+s(x2), data = dat, method = "REML")
#' b <- getViz(b)
#' 
#' # List layers available for parametric effect plot
#' o <- plot( pterm(b, 1) )
#' listLayers(o)
#' 
#' # List layers available for smooth effect plot
#' o <- plot( sm(b, 1) )
#' listLayers(o)
#' 
#' # List layers available for checking plot
#' o <- check1D(b, x1)
#' listLayers(o)
#'
#' @rdname listLayers
#' @export listLayers
#' 
listLayers <- function(o){

  if( !("plotSmooth" %in% class(o)) ){
    stop("listLayers works only with objects of class \"plotSmooth\"")
  }
  
  ty <- paste(o$type, collapse = '')
  
  lay <- ls(getNamespace("mgcViz"), all.names=TRUE)
  lay <- lay[ startsWith(lay, "l_") & endsWith(lay, paste0(".", ty)) ]
  
  if( length(lay) == 0 ) {  
    message(paste0("No layers available for objects of type ", ty)) 
    lay <- NULL
  } else {
    lay <- gsub(paste0(".", ty), '', lay)
  }
  
  # Layer l_gridQCheck2D needs to added manually, because it is implemented via l_gridCheck2D
  if( "l_gridCheck2D" %in% lay){ lay <- c(lay, "l_gridQCheck2D") }
  
  return(lay)

} 