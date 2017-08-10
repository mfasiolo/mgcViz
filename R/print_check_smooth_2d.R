
#' Printing output of check.mgcv.smooth.2D
#' 
#' @description XXX
#' @name print.check.smooth.2D
#' @rdname print.check.smooth.2D
#' @importFrom gridExtra grid.arrange
#' @export 
print.check.smooth.2D <- function(pl, lay = NULL, ...)
{
  if( is.null(lay) ){
    lay <- matrix(c(1, 1, 1, 2, 
                    1, 1, 1, 2, 
                    1, 1, 1, 2, 
                    3, 3, 3, 4), 4, 4)
  } 
  
  pl$pl3 <- pl$pl3 + coord_flip()
  
  pl <- lapply(pl, function(.inp) .inp+theme_bw())
  
  out <- grid.arrange(grobs=pl, layout_matrix=lay, ...)

  return( invisible(out) )
} 