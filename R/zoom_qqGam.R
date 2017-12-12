#' Efficiently zooming on GAM QQ-plots
#' 
#' @description This function allows to zoom into a QQ-plot produced by [qq.gamViz],
#'              in a computationally efficient manner.
#' @param o the output of \code{mgcViz::qq.gamViz}.
#' @param xlim if supplied then this pair of numbers are used as the x limits for the plot.
#' @param ylim if supplied then this pair of numbers are used as the y limits for the plot.
#' @param discrete if \code{TRUE} the QQ-plot is discretized into \code{ngr} bins before plotting,
#'                 in order to save plotting time (when the number of observations is large). If left
#'                 to \code{NULL}, the discretization is used if there are more than 10^4 observations.
#' @param ngr number of bins to be used in the discretization.
#' @param adGrid if \code{TRUE} the discretization grid is computed using the QQ-points falling within
#'               \code{xlim}. If \code{FALSE}, \code{zoom.qqGam} will compute \code{ngr} values using all
#'               the QQ-points used in the original \code{qq.gamViz} call (but only those falling within
#'               \code{xlim} and \code{ylim} will be plotted).
#' @param CI if \code{TRUE} confidence intervals are plotted.
#' @param worm if \code{TRUE} a worm-plot (a de-trended QQ-plot) is plotted, rather than a QQ-plot.
#' @param show.reps if \code{TRUE} all the QQ-lines corresponding to the simulated (model-based) QQ-plots.
#' @param a.qqpoi list of arguments to be passed to \code{ggplot2::geom_point}, which plots the main QQ-plot.
#' @param a.ablin list of arguments to be passed to \code{ggplot2::geom_abline}, which adds the reference line.
#' @param a.cipoly list of arguments to be passed to \code{ggplot2::geom_polygon}, which add the confidence intervals.
#' @param a.replin list of arguments to be passed to \code{ggplot2::geom_line}, which adds a line for each simulated
#'                 QQ-plot.
#' @param ... currently unused.
#' @name zoom.qqGam
#' @examples 
#' library(mgcViz);
#' set.seed(0)
#' n.samp <- 500
#' dat <- gamSim(1,n=n.samp,dist="binary",scale=.33)
#' p <- binomial()$linkinv(dat$f) ## binomial p
#' n <- sample(c(1,3),n.samp,replace=TRUE) ## binomial n
#' dat$y <- rbinom(n,n,p)
#' dat$n <- n
#' lr.fit <- bam(y/n ~ s(x0) + s(x1) + s(x2) + s(x3)
#'               , family = binomial, data = dat,
#'               weights = n, method = "REML")
#' lr.fit <- getViz(lr.fit)
#' 
#' set.seed(414)
#' o <- qq(lr.fit, rep = 50, method = "simul1", CI = "normal")
#' o # This is the whole qqplot
#' 
#' # We can zoom in along x at little extra costs (most computation already done by qq.gamViz)
#' zoom(o, xlim = c(0, 1), show.reps = TRUE, 
#'      a.replin = list(alpha = 0.1), a.qqpoi =  list(shape = 19))
#' @rdname zoom.qqGam
#' @export zoom.qqGam
#' @export
#' 
zoom.qqGam <- function(o, xlim = NULL, ylim = NULL, discrete = NULL, ngr = 1e3,
                       adGrid = TRUE, CI = FALSE, 
                       worm = FALSE, show.reps = FALSE, 
                       a.qqpoi = list(), a.ablin = list(), a.cipoly = list(), 
                       a.replin = list(), ...) {
  
  a.all <- .argMaster("zoom.qqGam")
  for(nam in names(a.all)){
    a.all[[nam]] <- .argSetup(a.all[[nam]], get(nam), nam, verbose = FALSE)
  }
  
  P <- o$store
  # Subset data according to xlim
  if (!is.null(xlim) && adGrid) {
    good <- which(P$Dq > xlim[1] & P$Dq < xlim[2])
    P$Dq <- P$Dq[good]
    P$D  <- P$D[good]
    P$dm <- P$dm[good, ]
    P$conf <- P$conf[, good]
  }
  if(is.null(discrete)) discrete <- length(P$Dq) > 1e4 
  P <- .discretize.qq.gam(P = P, discrete = discrete, ngr = ngr,
                          CI = CI, show.reps = show.reps)
  .pl <- .plot.qq.gam(P = P, CI = CI, worm = worm, show.reps = show.reps,
                      xlimit = xlim, ylimit = ylim, a.all = a.all)
  return(.pl)
}


