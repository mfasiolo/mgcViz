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
#' @param maxpo maximum number of residuals points that will be used by layers such as
#'              \code{resRug()} and \code{resPoints()}. If number of datapoints > \code{maxpo},
#'              then a subsample of \code{maxpo} points will be taken.
#' @param trans monotonic function to apply to the smooth and residuals, before plotting.
#'              Monotonicity is not checked. 
#' @param ... currently unused.
#' @return An object of class \code{c("plotSmooth", "gg")}.
#' @name plot.si.smooth.1D
#' @rdname plot.si.smooth.1D
#' @export plot.si.smooth.1D
#' @export
#' 
plot.si.smooth.1D <- function(x, n = 100, xlim = NULL, maxpo = 1e4, trans = identity, inner = FALSE, ...)  {
  
  if( inner ){
    # 1) Prepare data
    P <- .prepareInnerSI(o = x, n = n, xlim = xlim, ...)
    
    # 2) Produce output object
    out <- .plot.si.smooth.1D.inner(x = P$smooth, P = P, trans = trans, maxpo = maxpo)
    
  } else {
    # 1) Prepare data
    P <- .prepareNested(o = x, n = n, xlim = xlim, ...)
    
    # 2) Produce output object
    out <- .plot.si.smooth.1D(x = P$smooth, P = P, trans = trans, maxpo = maxpo)
  }
  
  class(out) <- c("plotSmooth", "gg")
  
  return(out)
}

########################
#' @noRd
.plot.si.smooth.1D <- function(x, P, trans, maxpo) {
  
  .dat <- list()
  
  if ( !is.null(P$raw) ) {
    # Construct data.frame of partial residuals
    res <- data.frame("x" = as.vector(P$raw))

    # Exclude residuals falling outside boundaries
    .dat$res <- res[res$x >= P$xlim[1] & res$x <= P$xlim[2], , drop = FALSE]
    
    # Sample if too many points (> maxpo)  
    nres <- nrow( .dat$res )
    .dat$res$sub <- if(nres > maxpo) { 
      sample( c(rep(T, maxpo), rep(F, nres-maxpo)) )
    } else { 
      rep(T, nres) 
    }
  }
  
  .dat$fit <- data.frame(x = P$x, y = P$fit, ty = trans(P$fit), se = P$se)
  .dat$misc <- list(trans = trans)
  
  .pl <- ggplot(data = .dat$fit, mapping = aes(x = x, y = y)) + 
    labs(title = P$main, x = P$xlab, y = P$ylab) + theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  return( list("ggObj" = .pl, "data" = .dat, type = c("singleIndex", "1D")) )
}

########################
#' @noRd
.plot.si.smooth.1D.inner <- function(x, P, trans, maxpo) {
  
  .dat <- list()
  
  .dat$fit <- data.frame(x = as.factor(P$x), y = P$fit, ty = trans(P$fit), se = P$se)
  .dat$misc <- list(trans = trans)
  
  .pl <- ggplot(data = .dat$fit, mapping = aes(x = x, y = y)) + 
    labs(title = P$main, x = P$xlab, y = P$ylab) + theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  return( list("ggObj" = .pl, "data" = .dat, type = c("singleIndexInner", "1D")) )
}
