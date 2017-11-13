#' Printing output of check.gam
#' 
#' @description XXX
#' @name print.checkGam
#' @rdname print.checkGam
#' @importFrom gridExtra grid.arrange
#' @export
print.checkGam <- function(x, lay = NULL, ...)
{
  if( is.null(lay) ){
    lay <- matrix(c(1, 2,  
                    3, 4), 2, 2)
  } 
  
  x <- lapply(x, function(.inp) .inp+theme_bw())
  
  out <- grid.arrange(grobs=x, layout_matrix=lay, ...)
  
  return( invisible(out) )
} 