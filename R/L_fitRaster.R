#'
#' Adding raster representing the fitted effect
#' 
#' @description This layer adds a raster or heat-map representing a fitted multidimensional effect.
#'
#' @param pTrans a function from (0, 1) to (0, 1) which takes as input a p-value and returns a value,
#'               \code{alpha}, which will be passed on to [ggplot2::geom_raster], and will determine
#'               the opacity of the heat-map. The p-value quantifies the significance of the smooth
#'               effect at each location (x1, x2). By default \code{pTrans} returns 1, but if we 
#'               set it to, say, \code{pTrans = function(.p) .p<0.05} then the regions with p-values
#'               higher than 0.05 will disappear. The [zto1] function can be used to specify \code{pTrans}
#'               in a flexible way.
#' @param noiseup if \code{TRUE} the fitted effect, mu(x1, x2), will be perturbed with random
#'                noise before being plotted. That is, at each location (x1, x2) a random
#'                variable z(x1, x2) ~ N(0, mul * V(x1, x2)) will be added to mu(x1, x2). Here 
#'                V(x1, x2) is the estimated variance of mu(x1, x2) and \code{mul} 
#'                is a scalar multiplier (see next argument). This is useful for understanding in  
#'                which areas the smooth is more uncertain, as these areas will appear more noisy.
#' @param mul positive multiplier that scales the variance of the fitted effect. See the \code{noiseup}
#'            argument.
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_raster}.
#' @return An object of class \code{gamLayer}.
#' @seealso See [plot.mgcv.smooth.2D], [plot.sos.smooth] or [plotSlice] for examples.
#' @importFrom stats rnorm
#' @export l_fitRaster
#'
l_fitRaster <- function(pTrans = function(.p) 1, noiseup = FALSE, mul = 1, ...){
  arg <- list(...)
  # NB: it doesn't make sense to let users change "na.value" here, because they can do it 
  # using scale_fill_gradientn() anyway.
  arg$xtra <- list("pTrans" = pTrans, "noiseup" = noiseup, "mul" = mul, "na.value" = "grey")
  o <- structure(list("fun" = "l_fitRaster",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method 
#' @noRd
l_fitRaster.sos0 <- function(a){
  
  # White is better when plotting on sphere (there are no NAs inside the sphere).
  a$xtra$na.value <- "white"
  l_fitRaster.2D(a)
  
}


######## Internal method 
#' @noRd
l_fitRaster.2D <- l_fitRaster.sos1 <- l_fitRaster.MDslice <- function(a){
  
  xtra <- a$xtra
  a$xtra <- NULL
  
  # Add CI lines to data
  .dat <- a$data$fit
  .dat$p <- xtra$pTrans( 1 - pnorm(abs(.dat$z)/.dat$se) )
  .trans <- a$data$misc$trans
  
  # Add noise to fitted surface before transforming. For smooths on sphere and
  # soap smooths .dat$se will contain NAs in regions outside the boundary, hence here 
  # we are suppressing related warnings.
  if(xtra$noiseup){ 
    .dat$tz <- withCallingHandlers(.trans( .dat$z + rnorm(length(.dat$z), 0, xtra$mul*.dat$se) ) , 
                                   warning = function(w){ 
                                     if(any(grepl("NAs produced", w))){ 
                                       invokeRestart( "muffleWarning" )
                                     }
                                   })
  }
  a$data <- .dat
  
  out <- list()
  a$mapping  <- aes(fill = tz, alpha = p)
  out[[1]] <- do.call("geom_raster", a)
  out[[2]] <- scale_fill_gradientn(colours = viridis(50, begin = 0.2), 
                                   na.value = xtra$na.value, name = "s(x)") 
  out[[3]] <- scale_alpha_identity() 

  class(out) <- "listOfLayers"
  
  return( out )
}



  