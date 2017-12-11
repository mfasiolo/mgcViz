#'
#' Printing the output of check.gamViz
#' 
#' @description This method prints the output of [check.gamViz].
#' @param x the output of \code{check.gamViz}.
#' @param lay the \code{layout_matrix} passed to [gridExtra::grid.arrange].
#' @param ... further arguments to be passed to \code{grid.arrange}.
#' @return Returns the output of \code{grid.arrange}, invisibly.
#' @name print.checkGam
#' @rdname print.checkGam
#' @importFrom gridExtra grid.arrange
#' @export print.checkGam
#' @export
#' 
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