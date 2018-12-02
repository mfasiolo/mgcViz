#'
#' Printing plots of smooth effects
#' 
#' @description This method prints objects of class \code{plotSmooth}.
#' @param x an object of class \code{plotSmooth}.
#' @param ... currently unused.
#' @return Returns \code{NULL}, invisibly.
#' @name print.plotSmooth
#' @rdname print.plotSmooth
#' @export print.plotSmooth
#' @export
#' 
print.plotSmooth <- function(x, addLay = TRUE, ...) {
  
  # x is empty and we convert it to plotGam which will add layers
  if( is.null(x$empty) ) { x$empty <- TRUE }
  if( addLay && x$empty ) {  
    x  <- structure(list("plots" = list(x), "empty" = x$empty), 
                    "class" = c("plotGam", "gg"))
  } else { # No additional layers
    x <- x$ggObj
  }
  
  print(x) # Call either plot.plotGam or plot.ggplot
  
  return( invisible(NULL) )
  
}  