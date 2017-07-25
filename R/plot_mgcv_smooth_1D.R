#' Plotting one dimensional smooth effects
#' 
#' @param o 
#' @param residuals 
#' @param rug 
#' @param se 
#' @param n 
#' @param jit 
#' @param xlab 
#' @param ylab 
#' @param main 
#' @param ylim 
#' @param xlim 
#' @param shade 
#' @param shade.col 
#' @param shift 
#' @param trans 
#' @param seWithMean 
#' @param unconditional 
#' @param by.resids 
#' @param resDen 
#' @param ngr 
#' @param bw 
#' @param tol 
#' @param alpDen 
#' @param paletteDen 
#' @param ... 
#'
#' @description XXX
#' @name plot.mgcv.smooth.1D
#' @examples 
#' library(mgcViz)
#' n  <- 1e3
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#' dat <- data.frame("x1" = x1, "x2" = x2,
#'                   "y" = sin(x1) + 0.5 * x2^2 + pmax(x2, 0.2) * rnorm(n))
#' b <- bam(y ~ s(x1)+s(x2), data = dat, method = "fREML", discrete = TRUE)
#' v <- getViz(b)
#' plot(v(1), rug = TRUE, resDen = "cond", residuals = TRUE)
#' plot(v(1), rug = FALSE, resDen = "cond", residuals = TRUE)
#' plot(v(1), rug = TRUE, resDen = "none", residuals = FALSE)
#' plot(v(1), rug = TRUE, resDen = "cond", residuals = TRUE)
#' plot(v(1), xlab = "XLAB", ylab = "YLAB", main = "TEST", shade = TRUE)
#' @rdname plot.mgcv.smooth.1D
#' @export plot.mgcv.smooth.1D
plot.mgcv.smooth.1D <- function(o,  
                                # axis labels and size
                                args.axis = list(main = NULL, xlab = NULL, ylab = NULL,
                                                 ylim = NULL, xlim = NULL),
                                # residuals layer
                                args.residuals = list(residuals = FALSE, color = "black",
                                                      by.resids = FALSE, pch = ".",
                                                      resDen = "none", ngr = c(50, 50),
                                                      bw = NULL, tol = 1e-6, alpDen = 0.7, 
                                                      paletteDen = viridis(50, begin = 0.2)),
                                # rug layer
                                args.rug = list(rug = TRUE, color = "black", jit = FALSE, size = 0.2),
                                # ci layer
                                args.ci = list(se = TRUE, shade = FALSE,
                                               color.shade = "gray80", color.lines = "gray50"),
                                # other args
                                n = 100, maxpo = 1e4, shift = 0, trans = I, seWithMean = FALSE, unconditional = FALSE, ...) {
  for (k in c(args.axis, args.residuals, args.rug, args.ci)) {
    stopifnot(is.list(k))
  }
  resDen <- match.arg(args.residuals$resDen, c("none", "cond", "joint"))
  o$smooth <- o$gObj$smooth[[o$ism]]
  fv.terms <- o$store$termsFit[ , o$store$np + o$ism]
  init <- .initializeXXX(o, unconditional, args.residuals$residuals,
                         args.residuals$resDen, args.ci$se, fv.terms)
  # Prepare for plotting
  tmp <- .createP(sm = init$o$smooth, x = init$o$gObj,
                  partial.resids = init$partial.resids,
                  se = init$se, n = n, n2 = NULL,
                  xlab = args.axis$xlab, ylab = args.axis$ylab, main = args.axis$main, 
                  # label = term.lab,
                  ylim = args.axis$ylim, xlim = args.axis$xlim, too.far = NULL, 
                  se1.mult = init$se1.mult, se2.mult = init$se2.mult,
                  seWithMean = seWithMean, fitSmooth = init$fv.terms,
                  w.resid = init$w.resid, resDen = args.residuals$resDen, ...)
  attr(o$smooth, "coefficients") <- tmp[["coef"]]
  # Plotting
  .ggobj <- .plot.mgcv.smooth.1D(x = init$o$smooth,
                                 P = tmp[["P"]],
                                 partial.resids = init$partial.resids,
                                 rug = args.rug$rug,
                                 se = init$se,
                                 scale = FALSE,
                                 n = n,
                                 maxpo = maxpo,
                                 jit = jit,
                                 shade = args.ci$shade,
                                 shade.col = args.ci$shade.col,
                                 ylim = ylim,
                                 shift = shift,
                                 trans = trans,
                                 by.resids = by.resids,
                                 resDen = resDen,
                                 ngr = ngr,
                                 bw = bw,
                                 tol = tol,
                                 alpDen = alpDen,
                                 alpha.rug = alpha.rug,
                                 paletteDen = paletteDen, ...)
  # add theme to plot, can be overriden later
  .ggobj <- .ggobj + theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  attr(.ggobj, "rawData") <- pd
  return(.ggobj)
}

# # axis labels and size
# args.axis = list(main = NULL, xlab = NULL, ylab = NULL,
#                  ylim = NULL, xlim = NULL),
# # residuals layer
# args.residuals = list(residuals = FALSE, res.color = "black",
#                       by.resids = FALSE, pch = ".",
#                       resDen = "none", ngr = c(50, 50),
#                       bw = NULL, tol = 1e-6, alpDen = 0.7, 
#                       paletteDen = viridis(50, begin = 0.2)),
# # rug layer
# args.rug = list(rug = TRUE, rug.color = "black", jit = FALSE, size = 0.2),
# # ci layer
# args.ci = list(se = TRUE, shade = FALSE,
#                color.shade = "gray80", color.lines = "gray50"),
# # other args
# n = 100, shift = 0, trans = I, seWithMean = FALSE, unconditional = FALSE, ...

.plot.mgcv.smooth.1D <- function(x, P, 
                                 # rug layer
                                 args.rug = list(rug = TRUE, color = "black", jit = FALSE, size = 0.2),
                                 # ci layer
                                 # residuals layer
                                 args.residuals = list(residuals = FALSE, color = "black",
                                                       by.resids = FALSE, pch = ".",
                                                       resDen = "none", ngr = c(50, 50),
                                                       bw = NULL, tol = 1e-6, alpDen = 0.7,
                                                       paletteDen = viridis(50, begin = 0.2)),
                                 # axis layer
                                 partial.resids = FALSE,
                                 se = TRUE, n = 100, maxpo = 1e4,
                                 jit = FALSE, shade = FALSE, shade.col = I("gray80"),
                                 ylim = NULL, shift = 0, trans = I, ...) {
  ul <- P$fit + P$se ## upper CL
  ll <- P$fit - P$se ## lower CL  
  if (is.null(ylim)) { # Calculate ylim of plot
    ylimit <- range(c(
      if (partial.resids || (resDen != "none")) {
        P$p.resid 
      } else {
        P$fit 
      }, 
      if(se){     # if standard error included
        c(ul, ll) # we add upper and lower limit to the ylim calculus
      }), na.rm = TRUE) 
  }
<<<<<<< HEAD
  
  ylimit <- if (is.null(ylim)){ ylimit <- ylimit + shift } else { ylim }
  
  .pl <- ggplot(data=data.frame(x=P$x, y=trans(P$fit+shift), uci=trans(ul+shift), lci=trans(ll-shift)), aes(x=x, y=y)) + 
    xlim(P$xlim[1], P$xlim[2]) + ylim(trans(ylimit[1]), trans(ylimit[2])) + labs(title = P$main, x = P$xlab, y = P$ylab)
  
  if( resDen != "none" && length(P$p.resid)  ){ # Plot conditional residual density
    if( is.null(dTrans) ){ dTrans <- function(.x){ .x^(1/3) } }
    
    .datR <- cbind(P$raw, trans(P$p.resid+shift))
=======
  ylimit <- if (is.null(ylim)) { # unless specified
    ylimit + shift               # we add the shift to ylim
  } else {
    ylim 
  }
  # base plot
  .pl <- ggplot(data    = 
                  data.frame(x = P$x,                  # x values
                             y = trans(P$fit + shift), # fitted + shift, after trans if necessary
                             uci = trans(ul + shift),  # upper confidence bound + shift & trans
                             lci = trans(ll - shift)), # lower confidence bound + shift & trans
                mapping = aes(x = x, y = y)) + 
    xlim(P$xlim[1], P$xlim[2]) +                       # xlim already calculated, from P
    ylim(trans(ylimit[1]), trans(ylimit[2])) +         # ylim 
    labs(title = P$main, x = P$xlab, y = P$ylab)       # add custom labels
  
  # density in background
  if (resDen != "none") { # Plot conditional residual density
    .datR <- cbind(P$raw, trans(P$p.resid + shift))
>>>>>>> spacing + more comments
    # Suppress warnings related to ngrid being too small relative to bw. Happens with big dataset.
    withCallingHandlers({
      if(is.null(bw)) {
        bw <- c(dpik(.datR[, 1], range.x = P$xlim, gridsize = ngr[1]), 
                dpik(.datR[, 2], range.x = ylimit, gridsize = ngr[2]))
      }
      estXY <- bkde2D(.datR, range.x = list(P$xlim, ylimit), gridsize = ngr, bandwidth = bw)
      if(resDen == "cond") { # Calculate conditional density of residuals | x
        estXY$fhat <- estXY$fhat / bkde(.datR[, 1], gridsize = ngr[1],
                                        range.x = P$xlim, bandwidth = bw[1])$y 
      }
    }, warning = function(w) invokeRestart("muffleWarning"))
    estXY$fhat[estXY$fhat <= tol * dnorm(0, 0, sd(.datR[, 2]))] <- NA 
    .pl <- .pl + 
      geom_raster(data = data.frame("d" = sqrt(as.numeric(t(estXY$fhat))), 
                                    "x" = rep(estXY$x1, each = ngr[1]), 
                                    "y" = rep(estXY$x2, ngr[2])),
                  mapping = aes(x = x, y = y, fill = d),
                  inherit.aes = FALSE, alpha = alpDen) + 
      scale_fill_gradientn(colours  = paletteDen,
                           na.value = "white")
  }
  
  # Add shade or lines for confidence bands
  if (se) {
    if (shade) { 
      .pl <- .pl +
        geom_polygon(data = data.frame("x" = c(P$x, P$x[n:1], P$x[1]),
                                       "y" = trans(c(ul, ll[n:1], ul[1]) + shift)), 
                     mapping = aes(x = x, y = y, fill = shade.col),
                     inherit.aes = FALSE)
    } else {
      .tmpF <- function(pl, ..., linetype = "dashed") # Alter default "linetype"
      {
        pl <- pl +
          geom_line(mapping  = aes(x = x, y = uci),
                    linetype = linetype, ...) + # upper ci line
          geom_line(mapping  = aes(x = x, y = lci),
                    linetype = linetype, ...)   # lower ci line
      }
      .pl <- .tmpF(.pl, ...)
    }
  }
  
  if( partial.resids || rug ){
    nrs <- length( P$raw )
    ii <- if( nrs > maxpo ){ sample(1:nrs, maxpo) } else { 1:nrs }
    .datRes <- data.frame("resx" = P$raw[ii]) 
    if( !is.null(P$p.resid) ){ .datRes$resy <- trans(P$p.resid[ii]+shift) }
  }
  
  # Add partial residuals
  if (partial.resids && (by.resids | x$by == "NA")) { 
    if (length(P$raw) == length(P$p.resid)) {
      .tmpF <- function(..., shape = '.', col = "black") # Alter default shape and col
      {
        geom_point(data = .datRes, 
                   mapping = aes(x = resx, y = resy),
                   na.rm = TRUE,
                   shape = shape,
                   col = col, ...)
      }
      .pl <- .pl + .tmpF(...)
    } else {
      warning("Partial residuals do not have a natural x-axis location for linear functional terms")
    }
  }
  # Add rug
  if (rug) { 
    if( jit ){ .datRes$resx <- jitter(.datRes$resx) }
    .tmpF <- function(pl, ..., size = 0.2) # Alter default "size"
    {
      geom_rug(data = .datRes, 
               mapping = aes(x = x),
               inherit.aes = FALSE, size = size, alpha = alpha.rug,...)
    }
    .pl <- .pl + .tmpF(.pl, ...)
  } 
  # ??
  .pl <- .pl + geom_line(...)
  return( .pl )
} 
