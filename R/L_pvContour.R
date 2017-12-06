#'
#' Adding contour of p-values
#' 
#' @description This function adds contour lines proportional to the p-value 
#'              of a multidimensional smooth effects. It is useful for checking 
#'              where (across covariates x1 and x2) the fitted smooth is significantly
#'              different from zero.
#'
#' @param pTrans a transformation to be applied to the p-values before plotting.
#' @param ... graphical arguments to be passed to  \code{ggplot2::geom_contour}.
#' @return An object of class \code{gamLayer}.
#' @seealso See [plotDiff.mgcv.smooth.2D] and [plotDiff.sos.smooth] for examples.
#' @export l_pvContour
#'
l_pvContour <- function(pTrans = identity, ...){
  arg <- list(...)
  arg$xtra <- list("pTrans" = pTrans)
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
  .dat$p <- xtra$pTrans( 1 - pnorm(abs(.dat$z)/.dat$se) )
  a$data <- .dat
  
  a$mapping  <- aes(z = p)
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE}
  if( is.null(a$colour) ){ a$colour <- "black" }
  fun <- "geom_contour"
  out <- do.call(fun, a)
  return( out )
  
}