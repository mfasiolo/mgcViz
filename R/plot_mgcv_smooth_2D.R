#'
#' Plotting two dimensional smooth effects
#' 
#' @description XXX
#' @param ... currently unused.
#' @name plot.mgcv.smooth.2D
#' @examples 
#' library(mgcViz)
#' set.seed(2) ## simulate some data...
#' dat <- gamSim(1, n = 1000, dist = "normal", scale = 2)
#' b <- gam(y ~ s(x0) + s(x1, x2) + s(x3), data = dat, method = "REML")
#' b <- getViz(b)
#' 
#' # Plot 2D effect with noised-up raster, contour and rug for design points 
#' # Opacity is proportional to the significance of the effect
#' plot(sm(b, 2)) + l_fitRaster(pFun = zto1(0.05, 2, 0.1), noiseup = T) + 
#'   l_rug() + l_fitContour()  
#' 
#' # Plot contour of effect joint density of design points
#' plot(sm(b, 2)) + l_dens(type = "joint") + l_points() + l_fitContour() + 
#'   coord_cartesian(expand = FALSE) # Fill the plot
#' @rdname plot.mgcv.smooth.2D
#' @export plot.mgcv.smooth.2D
#' 
plot.mgcv.smooth.2D <- function(o, n = 40, maxpo = 1e4, too.far = 0.1, trans = I, 
                                seWithMean = FALSE, unconditional = FALSE, ...) {
  
  # 1) Prepare data
  P <- .prepareP(o = o, unconditional = unconditional, residuals = TRUE, 
                 resDen = "none", se = TRUE, se.mult = 1, n = NULL, n2 = n,  
                 xlab = NULL, ylab = NULL, main = NULL, ylim = NULL, xlim = NULL,
                 too.far = too.far, seWithMean = seWithMean)
  
  # 2) Produce output object
  out <- .plot.mgcv.smooth.2D(x = P$smooth, P = P, trans = trans, maxpo = maxpo)
  
  class(out) <- c("plotSmooth", "2D")
  
  return(out)
  
}


############### Internal function
#' @noRd
.plot.mgcv.smooth.2D <- function(x, P, trans, maxpo) {
  
  .dat <- list()
  # 1) Build dataset on fitted effect
  P$fit[P$exclude] <- NA
  .dat$fit <- data.frame("z" = P$fit,
                         "tz" = trans(P$fit),
                         "x" = rep(P$x, length(P$fit) / length(P$x)), 
                         "y" = rep(P$y, each = length(P$fit) / length(P$x)), 
                         "se" = P$se)
  
  # 2) Build dataset on residuals
  # Exclude points too far from current slice (relevant only when called by plot.mgcv.smooth.MD)
  if ( !is.null(P$exclude2) && any(P$exclude2) ){
    P$raw <- P$raw[ !P$exclude2,  ]  
  }
  .dat$res <- P$raw
  
  # Sample if too many points (> maxpo) 
  nres <- nrow( .dat$res )
  .dat$res$sub <- if(nres > maxpo) { 
    sample( c(rep(T, maxpo), rep(F, nres-maxpo)) )
  } else { 
    rep(T, nres) 
  }
  
  .dat$misc <- list("trans" = trans)
  
  .pl <- ggplot(data = .dat$fit, aes(x = x, y = y, z = z)) +
    labs(title = P$main, x = P$xlab, y = P$ylab) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  return( list("ggObj" = .pl, "data" = .dat) )
  
}
