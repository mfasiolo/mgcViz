#'
#' Add raster to smooth effect plot
#' 
#' @description XXX
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_raster}.
#' @return An object of class \code{gamLayer}.
#' @export l_fitRaster
#'
l_fitRaster <- function(pFun = function(.p) 1, noiseup = FALSE, mul = 1, ...){
  arg <- list(...)
  arg$xtra <- list("pFun" = pFun, "noiseup" = noiseup, "mul" = mul)
  o <- structure(list("fun" = "l_fitRaster",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}


######## Internal method 
#' @noRd
l_fitRaster.plotSmooth2D <- function(a){
  
  xtra <- a$xtra
  a$xtra <- NULL
  
  # Add CI lines to data
  .dat <- a$data$fit
  .dat$p <- xtra$pFun( 1 - pnorm(abs(.dat$z)/.dat$se) )
  .trans <- a$data$misc$trans
  
  if(xtra$noiseup){ # Add noise to fitted surface before transforming
    .dat$tz <- .trans( .dat$z + rnorm(length(.dat$z), 0, xtra$mul*.dat$se) ) 
  }
  a$data <- .dat

  out <- list()
  a$mapping  <- aes(fill = tz, alpha = p)
  out[[1]] <- do.call("geom_raster", a)
  out[[2]] <- scale_fill_gradientn(colours = viridis(50, begin = 0.2), na.value = "grey", name = "s(x)") 
  out[[3]] <- scale_alpha_identity() 

  class(out) <- "listOfLayers"
  
  return( out )
}



  