#' Plotting higher-dimensional smooth effects
#' 
#' @description XXX
#' @name plot.mgcv.smooth.MD
#' @examples 
#' ## 3D example
#' library(mgcViz)
#' n <- 1e3
#' x <- rnorm(n); y <- rnorm(n); z <- rnorm(n)
#' 
#' ob <- (x-z)^2 + (y-z)^2 + rnorm(n)
#' b <- gam(ob ~ s(x, y, z))
#' v <- getViz(b)
#' 
#' # Plot one 2D slice
#' plot( v(1), fix = c("z"=0), scheme = 2 ) 
#' 
#' ## 4D
#' n <- 5e3
#' x <- rnorm(n); y <- rnorm(n); z <- rnorm(n); z2 <- rnorm(n)
#' 
#' ob <- (x-z)^2 + (y-z)^2 + z2^3 + rnorm(n)
#' b <- bam(ob ~ s(x, y, z, z2), discrete = T)
#' v <- getViz(b)
#' 
#' # Plot one 2D slice
#' plot(v(1), fix = c("z"=0, "z2"=1), scheme = 2 ) 
#'
#' @rdname plot.mgcv.smooth.MD
#' @importFrom mvnfast maha
#' @export plot.mgcv.smooth.MD

plot.mgcv.smooth.MD <- function(o, fix, residuals = FALSE, rug = TRUE, se = TRUE, n = 40, maxpo = 1e4,
                                pers = FALSE, theta = 30, phi = 30, xlab = NULL, ylab = NULL,
                                main = NULL, ylim = NULL, xlim = NULL, too.far = c(0.1, NA), se.mult = 1,
                                shift = 0, trans = I, seWithMean = FALSE, 
                                unconditional = FALSE, by.resids = FALSE,
                                scheme = 0, hcolors = viridis(50, begin = 0.2),
                                contour.col = 1, noiseup = FALSE, pFun = function(.p) 1, ...) {
  if (length(scheme) > 1){ 
    scheme <- scheme[1]
    warning("'scheme' should be a single number")
  }
  if (!(scheme %in% 0:4)){
    stop("'scheme' must be in 0:4")
  }
  if ( length(too.far) == 1 ){ too.far <- c(too.far, NA)  }
  o$smooth <- o$gObj$smooth[[o$ism]]
  resDen <- "none"
  fv.terms <- o$store$termsFit[ , o$store$np + o$ism]
  init <- .initializeXXX(o, unconditional, residuals, resDen, se, fv.terms)
  # affect initialize output
  o <- init$o
  w.resid <- init$w.resid
  partial.resids <- init$partial.resids
  se2.mult <- init$se2.mult
  se1.mult <- init$se1.mult
  se <- init$se
  fv.terms <- init$fv.terms
  order <- init$order
  # Prepare for plotting
  tmp <- .createP(sm = o$smooth, x = o$gObj, partial.resids = partial.resids,
                  se = se, n = NULL, n2 = n,
                  xlab = xlab, ylab = ylab, main = main,
                  ylim = ylim, xlim = xlim, too.far = too.far,
                  se1.mult = se.mult, se2.mult = se.mult, 
                  seWithMean = seWithMean, fitSmooth = fv.terms,
                  w.resid = w.resid, resDen = resDen, fix = fix, ...)
  pd <- tmp[["P"]]
  attr(o$smooth, "coefficients") <- tmp[["coef"]]
  rm(tmp)
  # Plotting
  .ggobj <- .plot.mgcv.smooth.2D(x = o$smooth, P = pd, partial.resids = partial.resids,
                                 rug = rug, se = se, scale = FALSE, n2 = n, maxpo = maxpo,
                                 pers = pers, theta = theta, phi = phi, jit = NULL,
                                 main = main, too.far = too.far, 
                                 shift = shift, trans = trans, by.resids = by.resids,
                                 scheme = scheme, hcolors = hcolors,
                                 contour.col = contour.col, noiseup = noiseup, pFun = pFun, ...)
  if (inherits(.ggobj, "ggplot")) {
    .ggobj <- .ggobj + theme_bw()
    attr(.ggobj, "rawData") <- pd
    return(.ggobj)
  } else {
    return(invisible(.ggobj))
  }
  
}
