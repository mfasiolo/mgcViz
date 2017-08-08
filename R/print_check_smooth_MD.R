#' Printing output of check.mgcv.smooth.MD
#' 
#' @description XXX
#' @name print.check.smooth.MD
#' @rdname print.check.smooth.MD
#' @importFrom gridExtra grid.arrange
#' @export 
print.check.smooth.MD <- function(pl, lay = NULL, ...)
{
  
  class( pl ) <- "check.smooth.2D"
  
  return( print.check.smooth.2D(pl = pl, lay = lay, ...) )
  
} 