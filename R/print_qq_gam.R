#'
#' Printing the output of qq.gam
#' 
#' @description This method prints the output of [mgcViz::qq.gam].
#' 
#' @param x an object of class \code{qqGam}.
#' @param ... currently unused.
#' @return Returns \code{NULL}, invisibly.
#' @name print.qqGam
#' @rdname print.qqGam
#' @export 
print.qqGam <- function(x, ...) {
  print(x$ggObj)
  return(invisible(NULL))
} 