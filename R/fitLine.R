#'
#' Add fitted smooth effect curve
#' 
#' @description XXX
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_line} (for 1D smooths) or 
#'            \code{ggplot2::geom_contour} (for 2D smooths).
#' @return An object of class \code{gamLayer}.
#' @export fitLine
#'
fitLine <- function(...){
  arg <- list(...)
  o <- structure(list("fun" = "fitLine",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method 
#' @noRd
fitLine.plotSmooth1D <- function(a){
  
  a$data <- a$data$fit
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE}
  fun <- "geom_line"
  out <- do.call(fun, a)
  return( out )
  
}

######## Internal method 
#' @noRd
fitLine.plotSmooth2D <- function(a){
  
  a$data <- a$data$fit
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE}
  if( is.null(a$colour) ){ a$colour <- "black" }
  fun <- "geom_contour"
  out <- do.call(fun, a)
  return( out )
  
}