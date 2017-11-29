#'
#' Adding contour of p-values to 2D smooth effect plot
#' 
#' @description Adding contour of p-values to 2D smooth effect plot
#'
#' @param ... graphical arguments to be passed to  \code{ggplot2::geom_contour}.
#' @return An object of class \code{gamLayer}.
#' @export l_pvContour
#'
l_pvContour <- function(pFun = identity, ...){
  arg <- list(...)
  arg$xtra <- list("pFun" = pFun)
  o <- structure(list("fun" = "l_pvContour",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method 
#' @noRd
l_pvContour.2D <- l_pvContour.sos1 <- l_pvContour.sos0 <- l_pvContour.MDslice <- function(a){
  
  xtra <- a$xtra
  a$xtra <- NULL
  
  .dat <- a$data$fit
  .dat$p <- xtra$pFun( 1 - pnorm(abs(.dat$z)/.dat$se) )
  a$data <- .dat
  
  a$mapping  <- aes(z = p)
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE}
  if( is.null(a$colour) ){ a$colour <- "black" }
  fun <- "geom_contour"
  out <- do.call(fun, a)
  return( out )
  
}