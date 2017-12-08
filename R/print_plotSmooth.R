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
print.plotSmooth <- function(x, ...) {
  print(x$ggObj)
  return( invisible(NULL) )
}  