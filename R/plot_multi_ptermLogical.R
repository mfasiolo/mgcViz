
#' @rdname plot.ptermFactor
#' @export plot.multi.ptermLogical
#' @export
#'
plot.multi.ptermLogical <- function(x, ...) {
  
  x[[1]]$name <- paste0(x[[1]]$name, "TRUE") 
  plot.multi.ptermNumeric(x, ...) 
  
}