#'
#' Adding density estimate heatmap
#' 
#' @description This layer adds a 2D density estimate heat-map to a plot.
#'              For 1D effect plots, it adds either the conditional density of the partial
#'              residuals, \code{p(r|x)}, or the joint density \code{p(r, x)}. For 2D
#'              effect plots it adds either \code{p(x1|x2)} or \code{p(x1, x2)}, where 
#'              \code{x1} and \code{x2} are the relevant covariates.  
#' @param type for 1D effect plots, if set to "cond" then the conditional residual 
#'             density \code{p(r|x)} is plotted. If set to "joint" the 
#'             joint density of residuals, \code{p(r, x)}, is plotted. 
#'             The behaviour is similar for 2D effect plots, but \code{r} indicates 
#'             the second covariate, not the residuals.
#' @param n vector of two positive integers, indicating the number of grid points
#'          at which the density is evaluated on the x and y axes.
#' @param bw vector with two positive entries, indicating the bandwidth to be used
#'           by the kernel density estimator of \code{p(x1, x2)} along x1 and x2.
#' @param tol small positive numerical tolerance. The estimated density at a certain 
#'            location is set to \code{NA} (hence it will appear white) when it falls 
#'            below \code{tol/sqrt(2*pi*sig)}, where \code{sig} is the standard 
#'            deviation of the residuals. Set \code{tol} to -1 plot the density on 
#'            the whole x-y plane, no matter how low it is.
#' @param trans the density on x-y is transformed using this function before being plotted.
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_raster}.
#' @details The density function is estimated using the fast binned kernel density estimation
#'          methods provided by the \code{KernSmooth} package, hence this function should be
#'          able to handle relatively large datasets (~ 10^6 observations).
#' @importFrom viridis viridis
#' @importFrom KernSmooth dpik bkde bkde2D
#' @return An object of class \code{gamLayer}.
#' @seealso See [plot.mgcv.smooth.1D], [plot.mgcv.smooth.2D] and [check1D] for examples.
#' @aliases l_dens
#' @export l_dens2D
#'
l_dens2D <- function(type, n = c(50, 50), bw = NULL, tol = 1e-6, trans = sqrt, ...){
  arg <- list(...)
  match.arg(type, c("cond", "joint"))
  arg$xtra <- list("type" = type, "n" = n, "bw" = bw, 
                   "tol" = tol, "trans" = trans, "grad" = list())
  o <- structure(list("fun" = "l_dens2D",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

#### Alias
#' @rdname l_dens2D
#' @export l_dens
l_dens <- l_dens2D

######## Internal method 
#' @noRd
l_dens2D.1D <- l_dens2D.Check1DNumeric <- l_dens2D.PtermNumeric <- 
  l_dens2D.nested1D <- function(a){
  
  xtra <- a$xtra
  a$xtra <- NULL
  
  if( is.null(a$data$res$y) ){ 
    message("l_dens2D(): Partial residuals are not available")  
    return( NULL )
  }
  
  # Computed joint or conditional residual density
  yv <- as.vector(a$data$res$y)
  M <- cbind(rep(a$data$res$x, length(yv) / nrow(a$data$res)), yv)
  
  dXY <- .fastKernDens(dat = M, xlimit = NULL, ylimit = NULL,
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
  out[[2]] <-  scale_fill_gradientn(colours = viridis(50, begin = 0.2), 
                                    na.value = "white", 
                                    name=ifelse(xtra$type == "cond", "p(y|x)", "p(x,y)"))
  class(out) <- "listOfLayers"
  
  return( out )
}

######## Internal method 
#' @noRd
l_dens2D.2D <- l_dens2D.Check2DNumericNumeric <- function(a){
 
  return( l_dens2D.1D(a) )
  
}