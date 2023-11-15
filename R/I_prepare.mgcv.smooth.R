##
## Default plot preparation method for smooth objects `x' inheriting from "mgcv.smooth"
## Input:
## `x' is a smooth object, usually part of a `gam' fit. It has an attribute
##     'coefficients' containg the coefs for the smooth, but usually these
##     are not needed.
## Output is a list of plot data containing:
##     * fit - the values for plotting 
##     * se.fit - standard errors of fit (can be NULL)
##     * the values against which to plot
##     * any raw data information
##     * any partial.residuals 

#' @description Default plot preparation method for smooth objects `x' 
#' inheriting from "mgcv.smooth".
#' @param x Is a smooth object, usually part of a `gam' fit. It has an attribute
#' `coefficients` containg the coefs for the smooth, but usually these
#' are not needed.
#' @param data 
#' @param n Number of points used for each 1-d plot - for a nice smooth plot
#'   this needs to be several times the estimated degrees of freedom for the
#'   smooth. Default value 100.
#' @param n2 Square root of number of points used to grid estimates of 2-d
#'   functions for contouring.
#' @param xlab If supplied then this will be used as the x label for all plots.
#' @param ylab If supplied then this will be used as the y label for all plots.
#' @param main Used as title (or z axis label) for plots if supplied.
#' @param ylim If supplied then this pair of numbers are used as the y limits
#'   for each plot.
#' @param xlim If supplied then this pair of numbers are used as the x limits
#'   for each plot.
#' @param too.far If greater than 0 then this is used to determine when a
#'   location is too far from data to be plotted when plotting 2-D smooths. This
#'   is useful since smooths tend to go wild away from data. The data are scaled
#'   into the unit square before deciding what to exclude, and too.far <- <-  is a
#'   distance within the unit square. Setting to zero can make plotting faster
#'   for large datasets, but care then needed with interpretation of plots.
#' @param ... Other graphics parameters to pass on to plotting commands. 
#' See details for smooth plot specific options.
#' @noRd
#' @export
.prepare.mgcv.smooth <- function(x, data = NULL, label = "", se1.mult = 1, se2.mult = 2,
                                 n = 100, n2 = 40,  xlab = NULL, ylab = NULL, 
                                 main = NULL, ylim = NULL, xlim = NULL, 
                                 too.far = 0.1, ...) {

  if(x$dim == 1) {
    out <- .preparePlotSmooth1D(x = x, data = data, label = label, se.mult = se1.mult,
                                n = n, xlim = xlim, xlab = xlab, 
                                ylab = ylab, main = main, ...)
  }
  
  if(x$dim == 2) {
    out <- .preparePlotSmooth2D(x = x, data = data, se.mult = se2.mult, n2 = n2, label = label,
                                xlab = xlab, ylab = ylab, main = main,
                                ylim = ylim, xlim = xlim, too.far = too.far, ...) 
  }
  
  if(x$dim > 2) {
    out <- .preparePlotSmoothMD(x = x, data = data, se.mult = se2.mult, n2 = n2, label = label,
                                xlab = xlab, ylab = ylab, main = main,
                                ylim = ylim, xlim = xlim, too.far = too.far, ...) 
  }
  
  return(out)
} 

