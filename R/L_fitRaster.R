#'
#' Add raster to smooth effect plot
#' 
#' @description XXX
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_raster}.
#' @return An object of class \code{gamLayer}.
#' @importFrom stats rnorm
#' @export l_fitRaster
#'
l_fitRaster <- function(pFun = function(.p) 1, noiseup = FALSE, mul = 1, ...){
  arg <- list(...)
  # NB: it doesn't make sense to let users change "na.value" here, because they can do it 
  # using scale_fill_gradientn() anyway.
  arg$xtra <- list("pFun" = pFun, "noiseup" = noiseup, "mul" = mul, "na.value" = "grey")
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
  .dat$p <- xtra$pFun( 1 - pnorm(abs(.dat$z)/.dat$se) )
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



  