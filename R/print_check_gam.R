#' Printing output of check.gam
#' 
#' @description XXX
#' @name print.check.gam
#' @rdname print.check.gam
#' @export 
print.check.gam <- function(pl, lay = NULL, ...)
{
  if( is.null(lay) ){
    lay <- matrix(c(1, 2,  
                    3, 4), 2, 2)
  } 
  
  pl <- lapply(pl, function(.inp) .inp+theme_bw())
  
  out <- grid.arrange(grobs=pl, layout_matrix=lay, ...)
  
  return( invisible(out) )
} 