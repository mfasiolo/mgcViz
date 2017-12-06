#'
#' Adding raster or heat-map of p-values
#' 
#' @description This function adds a raster or heat-map proportional to the p-value 
#'              of a multidimensional smooth effects. It is useful for checking 
#'              where (across covariates x1 and x2) the fitted smooth is significantly
#'              different from zero.
#'
#' @param pTrans a transformation to be applied to the p-values before plotting.
#' @param ... graphical arguments to be passed to  \code{ggplot2::geom_raster}.
#' @return An object of class \code{gamLayer}.
#' @seealso See [plotDiff.mgcv.smooth.2D] and [plotDiff.sos.smooth] for examples.
#' @export l_pvRaster
#'
l_pvRaster <- function(pTrans = identity, ...){
  arg <- list(...)
  # NB: it doesn't make sense to let users change "na.value" here, because they can do it 
  # using scale_fill_gradientn() anyway.
  arg$xtra <- list("pTrans" = pTrans, "na.value" = "grey")
  o <- structure(list("fun" = "l_pvRaster",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method 
#' @noRd
l_pvRaster.sos0 <- function(a){
  
  # White is better when plotting on sphere (there are no NAs inside the sphere).
  a$xtra$na.value <- "white"
  l_pvRaster.2D(a)
  
}

######## Internal method 
#' @noRd
l_pvRaster.2D <- l_pvRaster.sos1 <- l_pvRaster.MDslice <- function(a){
  
  xtra <- a$xtra
  a$xtra <- NULL
  
  .dat <- a$data$fit
  .dat$p <- xtra$pTrans( 1 - pnorm(abs(.dat$z)/.dat$se) )
  a$data <- .dat
  
  out <- list()
  a$mapping  <- aes(fill = p)
  out[[1]] <- do.call("geom_raster", a)
  out[[2]] <- scale_fill_gradientn(colours = viridis(50, begin = 0.2), 
                                   na.value = xtra$na.value, name = "p-val") 
  out[[3]] <- scale_alpha_identity() 
  
  class(out) <- "listOfLayers"
  
  return( out )
  
}



