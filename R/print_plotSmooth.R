#' Printing plots of smooth effects
#' 
#' @description XXX
#' @name print.plotSmooth
#' @rdname print.plotSmooth
#' @export 
print.plotSmooth <- function(o) {
  print(o$ggObj)
  return( invisible(NULL) )
}  