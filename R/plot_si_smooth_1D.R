#'
#' Plotting one dimensional single index effects
#' 
#' @description This method should be used to plot smooth effects 
#'              of class \code{"si.smooth.1D"}.
#' @param x a smooth effect object.
#' @param n number of grid points used to compute main effect and c.i. lines. 
#'          For a nice smooth plot this needs to be several times the estimated degrees of 
#'          freedom for the smooth.
#' @param xlim if supplied then this pair of numbers are used as the x limits for the plot.
#' @param trans monotonic function to apply to the smooth and residuals, before plotting.
#'              Monotonicity is not checked. 
#' @param ... currently unused.
#' @return An object of class \code{c("plotSmooth", "gg")}.
#' @name plot.si.smooth.1D
#' @rdname plot.si.smooth.1D
#' @export plot.si.smooth.1D
#' @export
#' 
plot.si.smooth.1D <- function(x, n = 100, xlim = NULL, trans = identity, ...)  {
  
  # 1) Prepare data
  P <- .prepareNested(o = x, n = n, xlim = xlim, ...)
  
  # 2) Produce output object
  out <- .plot.si.smooth.1D(x = P$smooth, P = P, trans = trans)
  
  class(out) <- c("plotSmooth", "gg")
  
  return(out)
}

########################
#' @noRd
.plot.si.smooth.1D <- function(x, P, trans) {
  
  .dat <- list()
  
  .dat$fit <- data.frame(x = P$x, y = P$fit, ty = trans(P$fit), se = P$se)
  .dat$misc <- list(trans = trans)
  
  .pl <- ggplot(data = .dat$fit, mapping = aes(x = x, y = y)) + 
    labs(title = P$main, x = P$xlab, y = P$ylab) + theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  return( list("ggObj" = .pl, "data" = .dat, type = c("singleIndex", "1D")) )
}
