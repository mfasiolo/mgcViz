
#' @rdname plot.ptermFactor
#' @export plot.ptermLogical
#' @export
#' 
plot.ptermLogical <- function(x, maxpo = 1e4, trans = identity, ...){
  
  plot.ptermFactor(x = x, maxpo = maxpo, trans = trans, ...)
  
}


