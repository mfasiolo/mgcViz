#' @description Internal function that does the actually plotting
#' inside [qq.gam] or [zoo.qqGam]
#' @param P A list, the output of `.discretize.qq.gam()`.
#' @param CI If TRUE, we want to compute also confidence intervals.
#' @param xlimit,ylimit X & Y axes ranges.
#' @param show.reps,rl.col,rep.col,rep.alpha,ci.col See [qq.gam()] documentation.
#' @return A ggplot object.
#' @noRd
.plot.qq.gam <- function(P, CI, show.reps, rl.col, rep.col, rep.alpha,
                         ci.col, shape, xlimit = NULL, ylimit = NULL, ...) {
  .pl <- ggplot()
  if (CI && !is.null(P$conf)) { # Add confidence intervals
    .pl <- .pl + geom_polygon(data = P$conf,
                            aes(x = x, y = y),
                            fill = ci.col)
  }
  if (show.reps && !is.null(P$dm)) { # Add a line for each simulation
    .pl <- .pl + geom_line(data = P$dm,
                         aes(x = x, y = y, group = id), 
                         colour = rep.col, alpha = rep.alpha)
  }
  .pl <- .pl +
    geom_abline(colour = rl.col) +
    geom_point(data = data.frame("sx" = P$Dq, "sy" = P$D),
               aes(x = sx, y = sy), shape = shape) +
    labs(x = "Theoretical quantiles", y = P$ylab,
         title = paste("Q-Q Plot, method =", P$method)) +
    coord_cartesian(xlim = xlimit, ylim = ylimit, expand = FALSE) +
    theme_bw()
  return(.pl)
}