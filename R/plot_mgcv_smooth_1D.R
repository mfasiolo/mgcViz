#' Plotting one dimensional smooth effects
#' 
#' @importFrom KernSmooth dpik bkde bkde2D
#' @importFrom viridis viridis
#' @description XXX
#' @name plot.mgcv.smooth.1D
#' @examples 
#' library(mgcViz)
#' library(viridis)
#' library(KernSmooth)
#' n  <- 1e3
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#' dat <- data.frame("x1" = x1, "x2" = x2,
#'                   "y" = sin(x1) + 0.5 * x2^2 + pmax(x2, 0.2) * rnorm(n))
#' b <- bam(y ~ s(x1)+s(x2), data = dat, method = "fREML", discrete = TRUE)
#' v <- getViz(b)
#' #n <- 100
#' plot(v(1))
#' plot(v(1), args.cilines = list(color = "blue", linetype = "dashed"), 
#'     args.dens = list(resDen = "cond", ngr = c(50, 50), # density layer
#'                      bw = NULL, tol = 1e-6, alpDen = 0.7,
#'                      colors = viridis(50, begin = 0.2), na.value = "white"))
#' plot(v(1), args.ci = list(shade = FALSE, se = TRUE))
#' 
#' # not working because of missing values - so geom_polygon fails...
#' plot(v(1),
#'      args.rug = list(rug = FALSE),
#'      args.cilines = list(color = "blue", linetype = "dashed"), 
#'      args.dens = list(resDen = "cond", ngr = c(50, 50), # density layer
#'                      bw = NULL, tol = 1e-6, alpDen = 0.7,
#'                      colors = viridis(50, begin = 0.2), na.value = "white"),
#'      args.axis = list(main = "TEST", xlab = "AXIS 1", ylab = "AXIS 2",
#'                       xlim = c(0, 4), ylim = c(0, 2)))
#' @rdname plot.mgcv.smooth.1D
#' @export plot.mgcv.smooth.1D
plot.mgcv.smooth.1D <- function(o, n = 100, maxpo = 1e4,
                                shift = 0, trans = I, unconditional = FALSE, residuals = TRUE, seWithMean = FALSE,
                                args.rug = list(rug = TRUE, jit = FALSE,  # rug layer
                                                colour = "black", size = 0.2, alpha = 1),
                                args.ci = list(se = TRUE, shade = TRUE), # ci lines layer
                                args.cilines = list(colour = "blue", linetype = "dashed"),
                                args.cipoly = list(colour = "light blue", alpha = 0.3), # ci shade layer
                                args.residuals = list(by.resids = FALSE,
                                                      colour = "black", shape = 46, na.rm = TRUE), # residuals layer
                                args.dens = list(resDen = "none", ngr = c(50, 50), # density layer
                                                 bw = NULL, tol = 1e-6, alpDen = 0.7,
                                                 colours = viridis(50, begin = 0.2), na.value = "white"),
                                args.axis = list(main = NULL, xlab = NULL, ylab = NULL,
                                                 xlim = NULL, ylim = NULL)) {
  resDen <- match.arg(args.dens$resDen, c("none", "cond", "joint"))
  o$smooth <- o$gObj$smooth[[o$ism]]
  fv.terms <- o$store$termsFit[ , o$store$np + o$ism]
  init <- .initializeXXX(o, unconditional, residuals,
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
                  w.resid = init$w.resid, resDen = args.dens$resDen)
  attr(o$smooth, "coefficients") <- tmp[["coef"]]
  # Plotting
  .ggobj <- .plot.mgcv.smooth.1D(x = init$o$smooth, P = tmp[["P"]],
                                 shift = shift, trans = trans, 
                                 args.rug = args.rug,
                                 args.ci = args.ci, # ci lines layer
                                 args.cilines = args.cilines,
                                 args.cipoly = args.cipoly, # ci shade layer
                                 args.residuals = c("partial.resids" = init$partial.resids,
                                                    args.residuals), # residuals layer
                                 args.dens = args.dens,
                                 args.axis = args.axis)
  # add theme to plot, can be overriden later
  .ggobj <- .ggobj + theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  attr(.ggobj, "rawData") <- tmp[["P"]]
  return(.ggobj)
}

#' @noRd
.plot.mgcv.smooth.1D <- function(
  x, P, shift = 0, trans = I, maxpo = 1e4, n = 100,
  args.rug = list(rug = TRUE, jit = FALSE,  # rug layer
                  color = "black", size = 0.2),
  args.ci = list(se = TRUE, shade = TRUE), # ci lines layer
  args.cilines = list(colour = "gray80", linetype = "dashed"),
  args.cipoly = list(colour = "gray80", fill = "gray80"), # ci shade layer
  args.residuals = list(partial.resids = TRUE, by.resids = FALSE,
                        colour = "black", shape = 46, na.rm = TRUE), # residuals layer
  args.dens = list(resDen = "none", ngr = c(50, 50), # density layer
                   bw = NULL, tol = 1e-6, alpDen = 0.7,
                   colors = viridis(50, begin = 0.2), na.value = "white"),
  args.axis = list(ylim = NULL), ...) { # axis layer
  # handling params ----
  # density
  args.dens.opts <- args.dens[names(args.dens) %in%
                                c("resDen", "bw", "tol", "alpDen", "ngr")]
  args.dens.grph <- args.dens[names(args.dens) %in%
                                c("colours", "values",
                                  "na.value", "space", "guide", "colors")]
  # residuals
  args.res.opts <- args.residuals[names(args.residuals) %in%
                                    c("partial.resids", "by.resids")]
  args.res.grph <- args.residuals[names(args.residuals) %in%
                                    c("fill", "colour", "shape", "na.rm", "size")]
  # rug
  args.rug.opts <- args.rug[names(args.rug) %in%
                              c("rug", "jit")]
  args.rug.grph <- args.rug[names(args.rug) %in%
                              c("color", "size")]
  # computations ----
  ul <- P$fit + P$se ## upper CL
  ll <- P$fit - P$se ## lower CL  
  if (is.null(args.axis$ylim)) { # Calculate ylim of plot
    ylimit <- range(c(
      if (args.res.opts$partial.resids || (args.dens.opts$resDen != "none")) {
        P$p.resid 
      } else {
        P$fit 
      }, 
      if(args.ci$se){     # if standard error included
        c(ul, ll) # we add upper and lower limit to the ylim calculus
      }), na.rm = TRUE) 
  }
  ylimit <- if (is.null(args.axis$ylim)) { # unless specified
    ylimit + shift               # we add the shift to ylim
  } else {
    args.axis$ylim 
  }
  # Compute conditional residual density
  if (args.dens.opts$resDen != "none") { # joint or cond
    .datR <- cbind(P$raw, trans(P$p.resid + shift))
    # Suppress warnings related to ngrid being too small relative to bw. Happens with big dataset.
    bw <- args.dens.opts$bw
    withCallingHandlers({
      if(is.null(bw)) {
        bw <- c(dpik(.datR[, 1], range.x = P$xlim, gridsize = args.dens.opts$ngr[1]), 
                dpik(.datR[, 2], range.x = ylimit, gridsize = args.dens.opts$ngr[2]))
      }
      estXY <- bkde2D(.datR, range.x = list(P$xlim, ylimit),
                      gridsize = args.dens.opts$ngr, bandwidth = bw)
      if(args.dens.opts$resDen == "cond") { 
        # Calculate conditional density of residuals | x
        estXY$fhat <- estXY$fhat / bkde(.datR[, 1], gridsize = args.dens.opts$ngr[1],
                                        range.x = P$xlim, bandwidth = bw[1])$y 
      }
    }, warning = function(w) invokeRestart("muffleWarning"))
    estXY$fhat[estXY$fhat <= args.dens.opts$tol * dnorm(0, 0, sd(.datR[, 2]))] <- NA 
  }
  # sample if too many points (> maxpo)
  if (args.res.opts$partial.resids || args.rug.opts$rug) {
    nrs <- length(P$p.resid)
    ii <- if (nrs > maxpo) {
      sample(1:nrs, maxpo)
    } else { 
      1:nrs 
    }
    .datRes <- data.frame(resx = as.vector(P$raw)[ii],
                          resy = trans(P$p.resid[ii] + shift))
  }
  # base plot ----
  dataB <- data.frame(x = P$x,                  # x values
                      y = trans(P$fit + shift), # fitted + shift, after trans if necessary
                      uci = trans(ul + shift),  # upper confidence bound + shift & trans
                      lci = trans(ll + shift)) # lower confidence bound + shift & trans
  .pl <- ggplot(data    = dataB,
                mapping = aes(x = x, y = y)) + 
    xlim(P$xlim[1], P$xlim[2]) +                       # xlim already calculated, from P
    ylim(trans(ylimit[1]), trans(ylimit[2])) +         # ylim 
    labs(title = P$main, x = P$xlab, y = P$ylab)      # add custom labels
  # (conditional|joint) density
  if (args.dens.opts$resDen != "none") {
    .pl <- .pl +
      geom_raster(data = data.frame("d" = sqrt(as.numeric(t(estXY$fhat))), 
                                    "x" = rep(estXY$x1, each = args.dens.opts$ngr[1]), 
                                    "y" = rep(estXY$x2, args.dens.opts$ngr[2])),
                  mapping = aes(x = x, y = y, fill = d),
                  inherit.aes = FALSE,
                  alpha = ifelse(is.null(args.dens.opts$alpDen),
                                 0.7, args.dens.opts$alpDen)) + 
      do.call(scale_fill_gradientn, args.dens.grph)
  }
  # Add shade or lines for confidence bands
  if (args.ci$se) {
    if (args.ci$shade) {
      .pl <- .pl +
        do.call(geom_polygon, c(list(
          data = data.frame("x" = c(dataB$x, dataB$x[n:1]),    # see below
                            "y" = c(dataB$uci, dataB$lci[n:1])),
          mapping = aes(x = x, y = y)),
          args.cipoly,
          inherit.aes = FALSE)
        )
    } 
    .pl <- .pl +
      do.call(geom_line, c(list(mapping = aes(x = x, y = uci)), args.cilines)) +
      do.call(geom_line, c(list(mapping = aes(x = x, y = lci)), args.cilines))
  }
  # Add partial residuals
  if (args.residuals$partial.resids &&
      (args.residuals$by.resids | x$by == "NA")) { 
    if (length(P$raw) == length(P$p.resid)) {
      .pl <- .pl + 
        do.call(geom_point,
                c(list(
                  data = .datRes, 
                  mapping = aes(x = resx, y = resy)),
                  args.res.grph)
        )
    } else {
      warning("Partial residuals do not have a natural x-axis location for linear functional terms")
    }
  }
  # Add rug
  if (args.rug.opts$rug) { 
    if (args.rug.opts$jit) {
      .datRes$resx <- jitter(.datRes$resx)
    }
    .pl <- .pl +
      do.call(geom_rug, c(list(
        data = .datRes, 
        mapping = aes(x = resx),
        inherit.aes = FALSE),
        args.rug.grph)
      )
  }
  # Add mean (or quantile) effect
  .pl <- .pl + geom_line()
  return(.pl)
} 
