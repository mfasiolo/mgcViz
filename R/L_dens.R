#'
#' Add density of partial residuals to plot
#' 
#' @description XXX
#'
#' @param type if set to "cond" then the conditional residual density is plotted.
#'             If set to "joint" the joint density of residuals and corresponding 
#'             covariate is plotted.
#' @param n vector of two positive integers, indicating the number of grid points
#'            at which the density is evaluated on the x and y axes.
#' @param bw vector with two positive entried, indicating the bandwidth to be used
#'           by the kernel density estimator along x and y.
#' @param tol small positive numerical tolerance. The estimated density at a certain 
#'            location is set to \code{NA} (hence it will appear white) when it falls 
#'            below \code{tol/sqrt(2*pi*sig)}, where \code{sig} is the standard 
#'            deviation of the residuals. Set \code{tol} to -1 plot the density on 
#'            the whole x-y plane, no matter how low it is.
#' @param trans the density on x-y is transformed using this function before being plotted.
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_raster}.
#' @importFrom viridis viridis
#' @importFrom KernSmooth dpik bkde bkde2D
#' @return An object of class \code{gamLayer}.
#' @export l_dens
#'
l_dens <- function(type, n = c(50, 50), 
                    bw = NULL, tol = 1e-6, trans = sqrt, ...){
  arg <- list(...)
  match.arg(type, c("cond", "joint"))
  arg$xtra <- list("type" = type, "n" = n, "bw" = bw, 
                   "tol" = tol, "trans" = trans, "grad" = list())
  o <- structure(list("fun" = "l_dens",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method 
#' @noRd
l_dens.plotSmooth1Dgg <- l_dens.plotSmoothCheck1Dgg <- l_dens.plotSmoothPtermNumericgg <- function(a){
  
  xtra <- a$xtra
  a$xtra <- NULL
  
  if( is.null(a$data$res$y) ){ 
    message("l_dens(): Partial residuals are not available")  
    return( NULL )
  }
  
  # Computed joint or conditional residual density
  dXY <- .fastKernDens(dat = a$data$res, xlimit = NULL, ylimit = NULL,
                       cond = (xtra$type == "cond"), bw = xtra$bw, ngr = xtra$n, 
                       tol = xtra$tol)$dXY
  
  # Add arguments for `geom_raster`
  a$data <- data.frame("d" = xtra$trans(as.numeric(t(dXY$fhat))),
                       "x" = rep(dXY$x1, each = xtra$n[1]),
                       "y" = rep(dXY$x2, xtra$n[2]))
  a$mapping <- aes(x = x, y = y, fill = d)
  a$inherit.aes <- FALSE
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE }

  # Build layers
  out <- list()
  out[[1]] <- do.call("geom_raster", a) 
  out[[2]] <-  scale_fill_gradientn(colours = viridis(50, begin = 0.2), na.value = "white") 
  
  class(out) <- "listOfLayers"
  
  return( out )
}

######## Internal method 
#' @noRd
l_dens.plotSmooth2Dgg <- l_dens.plotSmoothCheck2Dgg <- function(a){
 
  return( l_dens.plotSmooth1Dgg(a) )
  
}