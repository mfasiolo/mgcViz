#' Printing output of qq.gam
#' 
#' @description XXX
#' @name print.qqGam
#' @rdname print.qqGam
#' @export 
print.qqGam <- function(o) {
  print(o$ggPlot)
  return(invisible(NULL))
} 