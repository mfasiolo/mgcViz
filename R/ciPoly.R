#'
#' Add confidence band to smooth effect plot
#' 
#' @description XXX
#'
#' @param mul number multiplied by the standard errors when calculating 
#'            standard error curves or surfaces.
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_polygon}.
#' @return An object of class \code{gamLayer}
#' @export ciPoly
#'
ciPoly <- function(mul = 2, ...){
  arg <- list(...)
  arg$xtra <- list("mul" = mul)
  o <- structure(list("fun" = "ciPoly",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method 
#' @noRd
ciPoly.plotSmooth1D <- function(a){
  
  xtra <- a$xtra
  a$xtra <- NULL
  
  # Create dataframe for polygon
  .dat <- a$data$fit[ c("x", "y", "se") ]
  .trans <- a$data$misc$trans
  a$data <- data.frame("x" = c(.dat$x, rev(.dat$x)), 
                         "y" = c(.trans( .dat$y + xtra$mul * .dat$se ), 
                                 rev(.trans( .dat$y - xtra$mul * .dat$se ))))
  
  a$mapping  <- aes(x = x, y = y)
  a$inherit.aes <- FALSE
  if( is.null(a$fill) ){ a$fill <- "light grey"}
  
  out <- do.call("geom_polygon", a)
  
  return( out )
}