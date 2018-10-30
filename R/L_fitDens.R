#'
#' Adding density strip of fitted effect
#' 
#' @description This layer adds a conditional posterior density strip to 1D smooth effects plots.
#'              With the default colour scale, the opacity is proportional to the conditional density of the fitted
#'              effects, under the usual Gaussian approximation the posterior.
#' @param n sqrt of the number of grid points used to compute the effect plot.
#' @param level confidence level. By default the conditional density of the fit will be plotted
#'              between the Gaussian quantiles 0.025 and 0.975, hence the \code{level} determines the 
#'              width of the y-axis.
#' @param trans monotonic function to be applied to the density of the fit, which determines colour of 
#'              the plot. Monotonicity is not checked.  
#' @param ... further arguments to be passed to \code{ggplot2::geom_raster}.
#' @details See Bowman (2018) for explanations about the advantages of density strips, relative
#'         to plots including the mean fit + confidence intervals.
#' @references Bowman, D. W (2018). Graphics for uncertainty. Journal of the Royal Statistical Society: Series A.
#' @return An object of class gamLayer.   
#' @examples 
#' library(mgcViz)
#' set.seed(44)
#' dat <- gamSim(1,n=400,dist="normal",scale=2)
#' b <- gamV(y~s(x0)+x1+s(x2)+s(x3),data=dat)
#' 
#' plot(sm(b, 1)) + l_fitDens() + l_fitLine()
#' plot(pterm(b, 1)) + l_fitDens(trans = function(x) x^0.25) + l_fitLine()
#'                
#' @export l_fitDens
#'
l_fitDens <- function(n = 50, level = 0.95, trans = identity, ...){
  arg <- list(...)
  arg$xtra <- list("n" = n, "level" = level, "trans" = trans, "grad" = list())
  o <- structure(list("fun" = "l_fitDens",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method 
#' @noRd
l_fitDens.1D <- l_fitDens.PtermNumeric  <- function(a){
  
  xtra <- a$xtra
  a$xtra <- NULL
  
  .dat <- a$dat$fit
  
  ny <- xtra$n
  nx <- length(.dat$x)
  
  lev <- abs(qnorm(xtra$level))
  xgr <- rep(.dat$x, ny)
  ygr <- rep(seq(min(.dat$y - lev*.dat$se), max(.dat$y + lev*.dat$se), length.out = ny), each = nx)

  z <- dnorm(rep(.dat$y, ny), ygr, rep(.dat$se, ny))
  
  # Add arguments for `geom_raster`
  a$data <- data.frame("d" = xtra$trans(z),
                       "x" = xgr,
                       "y" = ygr)
  a$mapping <- aes(x = x, y = y, fill = d)
  a$inherit.aes <- FALSE
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE }
  
  # Build layers
  out <- list()
  out[[1]] <- do.call("geom_raster", a) 
  out[[2]] <-  scale_fill_gradient("low" = "white", high = "blue", na.value = "white") 
  
  class(out) <- "listOfLayers"
  
  return( out )
}
