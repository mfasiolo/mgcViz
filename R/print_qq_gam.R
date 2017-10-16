#' Printing output of qq.gam
#' 
#' @param x an object of class \code{qqGam}.
#' @param ... currently unused.
#' @name print.qqGam
#' @rdname print.qqGam
#' @export 
print.qqGam <- function(x, ...) {
  print(x$ggObj)
  return(invisible(NULL))
} 