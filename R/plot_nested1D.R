#'
#' Plotting one dimensional nested effects
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
#' @name plot.nested1D
#' @rdname plot.nested1D
#' @export plot.nested1D
#' @export
#' 
plot.nested1D <- function(x, n = 100, xlim = NULL, maxpo = 1e4, trans = identity, inner = FALSE, ...)  {
  
  if( inner ){
    # 1) Prepare data
    P <- .prepareInnerNested(o = x, n = n, xlim = xlim, ...)
    
    out <- .plot.si.inner.smooth.1D(P = P, trans = trans)
    
  } else {
    # 1) Prepare data
    P <- .prepareOuterNested(o = x, n = n, xlim = xlim, ...)
    
    # 2) Produce output object
    out <- .plot.outer.nested.1D(x = P$smooth, P = P, trans = trans, maxpo = maxpo)
  }
  
  class(out) <- c("plotSmooth", "gg")
  
  return(out)
}

########################
#' @noRd
.plot.outer.nested.1D <- function(x, P, trans, maxpo) {
  
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
  
  return( list("ggObj" = .pl, "data" = .dat, type = c("nested", "1D")) )
}

########################
#' @noRd
.plot.si.inner.smooth.1D <- function(P, trans) {
  
  .dat <- list()
  .dat$fit <- data.frame("x"  = as.factor(P$x),
                         "y"  = unname(P$fit),
                         "ty"  = trans( unname(P$fit) ),
                         "se" = unname(P$se) )
  .dat$misc <- list("trans" = trans)
  
  .pl <- ggplot(data = .dat$fit, aes("x" = x, "y" = ty)) +
    labs(title = P$main, x = P$xlab, y = P$ylab) +
    scale_x_discrete() +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
  
  return( structure(list("ggObj" = .pl, "data" = .dat, "type" = c("singleIndexInner", "Factor")), 
                    class = c("plotSmooth",  "gg")) )
  
}
