#' @description Internal function that does the actually plotting
#' inside [qq.gamViz] or [zoom.qqGam]
#' @param P A list, the output of `.discretize.qq.gam()`.
#' @param CI If TRUE, we want to compute also confidence intervals.
#' @param xlimit,ylimit X & Y axes ranges.
#' @param showReps,rl.col,rep.col,rep.alpha,ci.col See [qq.gamViz] documentation.
#' @return A ggplot object.
#' @noRd
.plot.qq.gam <- function(P, CI, worm, showReps, xlimit, ylimit, a.all, ...) {
  
  # Rotate everything if worm plot needed
  if( worm ){
    if (CI && !is.null(P$conf)) { P$conf$y <- P$conf$y - P$conf$x }
    if (showReps && !is.null(P$dm)){ P$dm$y <- P$dm$y - P$dm$x }
    P$D <- P$D - P$Dq
  } 
  
  .pl <- ggplot()
  if (CI && !is.null(P$conf)) { # Add confidence intervals
    .pl <- .pl + do.call("geom_polygon", 
                         c(list(data = P$conf, aes(x = x, y = y)), a.all$a.cipoly))
  }
  if (showReps && !is.null(P$dm)) { # Add a line for each simulation
    .pl <- .pl + do.call("geom_line", 
                         c(list(data = P$dm, aes(x = x, y = y, group = id)), a.all$a.replin))
  }
  .pl <- .pl +
    do.call("geom_abline", c(list(intercept = 0, slope = !worm), a.all$a.ablin)) +
    do.call("geom_point", 
            c(list(data = data.frame("sx" = P$Dq, "sy" = P$D), aes(x = sx, y = sy)), a.all$a.qqpoi)) +
    labs(x = "Theoretical quantiles", y = P$ylab,
         title = paste("Q-Q Plot, method =", P$method)) +
    coord_cartesian(xlim = xlimit, ylim = ylimit, expand = FALSE) +
    theme_bw()
  
  return(.pl)
  
}