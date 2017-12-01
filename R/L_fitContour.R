#'
#' Adding fitted effect contour lines
#' 
#' @description This layer adds the contour lines corresponding to a fitted multidimensional effect.
#'
#' @param ... graphical arguments to be passed to  \code{ggplot2::geom_contour}.
#' @return An object of class \code{gamLayer}.
#' @seealso See [plot.mgcv.smooth.2D], [plot.mgcv.smooth.MD], [plot.sos.smooth] and
#'          [plotSlice] for examples.
#' @export l_fitContour
#'
l_fitContour <- function(...){
  arg <- list(...)
  o <- structure(list("fun" = "l_fitContour",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method 
#' @noRd
l_fitContour.2D <- l_fitContour.sos1 <- 
l_fitContour.sos0 <- l_fitContour.MDslice <- function(a){
  
  a$data <- a$data$fit
  a$mapping  <- aes(z = tz)
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE}
  if( is.null(a$colour) ){ a$colour <- "black" }
  fun <- "geom_contour"
  out <- do.call(fun, a)
  return( out )
  
}