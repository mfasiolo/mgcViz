#' Plotting one dimensional smooth factor interactions
#' 
#' @description Plotting one dimensional smooth factor interactions
#' @name plot.fs.interaction.1D
#' @examples 
#' library(mgcv)
#' library(mgcViz)
#' set.seed(0)
#' ## simulate data...
#' f0 <- function(x) 2 * sin(pi * x)
#' f1 <- function(x,a=2,b=-1) exp(a * x)+b
#' f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 * 
#'   (10 * x)^3 * (1 - x)^10
#' n <- 500;nf <- 25
#' fac <- sample(1:nf,n,replace=TRUE)
#' x0 <- runif(n);x1 <- runif(n);x2 <- runif(n)
#' a <- rnorm(nf)*.2 + 2;b <- rnorm(nf)*.5
#' f <- f0(x0) + f1(x1,a[fac],b[fac]) + f2(x2)
#' fac <- factor(fac)
#' y <- f + rnorm(n)*2
#' ## so response depends on global smooths of x0 and 
#' ## x2, and a smooth of x1 for each level of fac.
#' 
#' ## fit model (note p-values not available when fit 
#' ## using gamm)...
#' bm <- gamm(y ~ s(x0)+ s(x1, fac, bs = "fs", k = 5) + s(x2, k = 20))
#' v <- getViz(bm$gam)
#' plot(v(2))
#' plot(v(2), ylim = c(-0.5, 0.5), xlim = c(0.25, 0.75))
#' @rdname plot.fs.interaction.1D
#' @export plot.fs.interaction.1D
#' 
plot.fs.interaction.1D <- function(o, n = 100, alpha = NULL, legend = TRUE,
                                   xlab = NULL, ylab = NULL, main = NULL,
                                   ylim = NULL, xlim = NULL,
                                   shift = 0, trans = I, ...) {
  o$smooth <- o$gObj$smooth[[o$ism]]
  unconditional <- se <- residuals <- FALSE
  resDen <- "none"
  fv.terms <- o$store$termsFit[ , o$store$np + o$ism]
  init <- .initializeXXX(o, unconditional, residuals, resDen, se, fv.terms)
  # Prepare for plotting
  tmp <- .createP(sm = init$o$smooth, x = o$gObj, partial.resids = init$partial.resids,
                  se = init$se, n = n, n2 = NULL,
                  xlab = xlab, ylab = ylab, main = main, 
                  ylim = ylim, xlim = xlim, too.far = NULL, 
                  se1.mult = NULL, se2.mult = NULL, 
                  seWithMean = NULL, fitSmooth = init$fv.terms,
                  w.resid = init$w.resid, resDen = resDen, ...)
  attr(o$smooth, "coefficients") <- tmp[["coef"]]
  # Plotting
  .ggobj <- .plot.fs.interaction.1D(x = o$smooth, P = tmp[["P"]], ylim = ylim,
                                    shift = shift, trans = trans, alpha = alpha, ...)
  if(!legend){
    .ggobj <- .ggobj + theme(legend.position="none")
  } 
  attr(.ggobj, "rawData") <- tmp[["P"]]
  .ggobj
}

# Internal function
.plot.fs.interaction.1D <- function(x, P = NULL, ylim = NULL, shift = 0,
                                    trans = I, alpha = NULL, ...) {
  .dat <- data.frame("x"  = rep(P$x, P$nf),
                     "y"  = trans(P$fit + shift),
                     "id" = as.factor(rep(x$flev, each = P$n)))
  if (is.null(ylim)) {
    ylim <- range(.dat$y) 
  }
  if (is.null(xlim)) {
    xlim <- range(.dat$x) 
  }
  if (is.null(alpha)){
    alpha <- 
      if (P$nf < 10){
        1
      } else if (P$nf < 100) {
        0.5
      } else {
        0.3
      }
  }
  .pl <- 
    ggplot(data = .dat, aes("x" = x, "y" = y, "colour" = id)) +
    geom_line(alpha = alpha) + 
    labs(title = P$main, x = P$xlab, y = P$ylab) +
    ylim(ylim[1], ylim[2]) +
    xlim(xlim[1], xlim[2])
  return(.pl)
} ## end plot.fs.interaction

