#' Plotting one dimensional smooth effects
#' 
#' @importFrom KernSmooth dpik bkde bkde2D
#' @importFrom viridis viridis
#' @description Plotting one dimensional smooth effects.
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
#' plot(v(1))
#' plot(v(1), resDen = "cond",  a.cilin = list(colour = "blue", linetype = "dashed"))
#' plot(v(1), a.ci = list(shade = FALSE))
#' plot(v(1), rug = TRUE, resDen = "none", main = "TEST", xlab = "AXIS 1", ylab = "AXIS 2",
#'      xlim = c(0, 4), ylim = c(0, 2),  
#'      a.rug = list(jit = TRUE, colour = "orange", alpha = 0.5),
#'      a.cili = list(size = 3), a.res = list(shape = 15, size = 0.4),
#'      a.cipoly = list(colour = "gray80", fill = "gray80", alpha = 1))
#' plot(v(1), rug = TRUE, main = "TEST", xlab = "AXIS 1", ylab = "AXIS 2",
#'      xlim = c(-10, 10), ylim = c(-10, 10),
#'      a.rug = list(colour = "orange", alpha = 0.5),
#'      a.cilin = list(size = 3), 
#'      a.res = list(shape = 15, size = 0.4),
#'      a.cipoly = list(colour = "gray80", fill = "gray80", alpha = 1))
#' @rdname plot.mgcv.smooth.1D
#' @export plot.mgcv.smooth.1D
plot.mgcv.smooth.1D <- function(o, n = 100, maxpo = 1e4,
                                shift = 0, trans = I, residuals = TRUE,
                                rug = TRUE, se = TRUE, resDen = "none", xlim = NULL, ylim = NULL,
                                main = NULL, xlab = NULL, ylab = NULL,
                                a.rug = list(), a.ci = list(), a.cilin = list(), a.cipoly = list(),
                                a.res = list(), a.dens = list()) {
  
  # 1) Deal with arguments ----
  # Get internal arguments lists 
  a.all <- .argMaster(as.character(match.call()[[1]])) 
  
  # Matching internal and external lists
  for(nam in names(a.all)){
    a.all[[nam]] <- .argSetup(a.all[[nam]], get(nam), nam, verbose = FALSE)
  }
  
  resDen <- match.arg(resDen, c("none", "cond", "joint"))
  
  # 2) Prepare for plotting ----
  o$smooth <- o$gObj$smooth[[o$ism]]
  fv.terms <- o$store$termsFit[ , o$store$np + o$ism]
  init <- .initializeXXX(o, a.all$a.ci$unconditional, residuals, resDen, se, fv.terms)
  
  tmp <- .createP(sm = init$o$smooth, x = init$o$gObj,
                  partial.resids = init$partial.resids,
                  se = init$se, n = n, n2 = NULL,
                  xlab = xlab, ylab = ylab, main = main, 
                  # label = term.lab,
                  ylim = ylim, xlim = xlim, too.far = NULL, 
                  se1.mult = init$se1.mult, se2.mult = init$se2.mult,
                  seWithMean = a.all$a.ci$seWithMean, fitSmooth = init$fv.terms,
                  w.resid = init$w.resid, resDen = resDen)
  attr(o$smooth, "coefficients") <- tmp[["coef"]]
  
  # 3) Plotting ----
  .ggobj <- .plot.mgcv.smooth.1D(x = init$o$smooth, P = tmp[["P"]], n = n, se = se,
                                 shift = shift, trans = trans, maxpo = maxpo, partial.resids = init$partial.resids,
                                 resDen = resDen, ylim = ylim, rug = rug, a.all = a.all)
  attr(.ggobj, "rawData") <- tmp[["P"]]
  return(.ggobj)
}

#' @noRd
.plot.mgcv.smooth.1D <- function(x, P, n, se, shift, trans, maxpo,
                                 partial.resids, resDen, ylim, rug, a.all) { # axis layer
  # handling params ----
  # Splitting each argument list in options 'o' and graphical 'g'. Notice the '<<-'.
  env <- environment()
  with(a.all, {
    tmp <- c("o", "g")
    assign("a.rug", .splitList(a.rug, g = c("rug", "jit"), n = tmp), envir = env)
    assign("a.dens", .splitList(a.dens, g = c("bw", "tol", "ngr", "alpDen"), n = tmp), envir = env)
    assign("a.res", .splitList(a.res, g = c("by.resids"), n = tmp), envir = env)
    assign("a.ci", a.ci, envir = env)
    assign("a.cilin", a.cilin, envir = env)
    assign("a.cipoly", a.cipoly, envir = env)
  })
  
  # computations ----
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
  ylimit <- if (is.null(ylim)) { # unless specified
    ylimit + shift               # we add the shift to ylim
  } else {
    ylim 
  }
  
  # Compute conditional residual density
  if (resDen != "none") { # joint or cond
    dXY <- do.call(".fastKernDens", c(list("dat" = cbind(P$raw, trans(P$p.resid + shift)),
                                           "xlimit" = P$xlim, "ylimit" = ylimit,
                                           "cond" = (resDen == "cond")), 
                                      a.dens$o))$dXY
  }
  # sample if too many points (> maxpo)
  if (partial.resids || rug) {
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
  .dataB <- data.frame(x = P$x,                  # x values
                       y = trans(P$fit + shift), # fitted + shift, after trans if necessary
                       uci = trans(ul + shift),  # upper confidence bound + shift & trans
                       lci = trans(ll + shift)) # lower confidence bound + shift & trans
  .pl <- 
    ggplot(data    = .dataB,
           mapping = aes(x = x, y = y)) + 
    coord_cartesian(xlim = P$xlim, ylim = trans(ylimit)) +         # ylim 
    labs(title = P$main, x = P$xlab, y = P$ylab)      # add custom labels
  
  # (conditional|joint) density
  if (resDen != "none") {
    .pl <- .pl +
      geom_raster(data = data.frame("d" = sqrt(as.numeric(t(dXY$fhat))), 
                                    "x" = rep(dXY$x1, each = a.dens$o$ngr[1]), 
                                    "y" = rep(dXY$x2, a.dens$o$ngr[2])),
                  mapping = aes(x = x, y = y, fill = d),
                  inherit.aes = FALSE,
                  alpha = ifelse(is.null(a.dens$o$alpDen),
                                 0.7,  a.dens$o$alpDen)) + 
      do.call(scale_fill_gradientn, a.dens$g)
  }
  
  # NB adding shade for CI before residuals, but adding CI lines after residuals
  if (se && a.ci$shade) {
    .dataPoly <- .dataB
    .dataPoly$uci[.dataPoly$uci > trans(ylimit[2])] <- ylimit[2]
    .dataPoly$lci[.dataPoly$lci < trans(ylimit[1])] <- ylimit[1]
    .pl <- .pl +
      do.call(geom_polygon, c(list(
        data = data.frame("x" = c(.dataPoly$x, .dataPoly$x[nrow(.dataPoly):1]),   
                          "y" = c(.dataPoly$uci, .dataPoly$lci[nrow(.dataPoly):1])),
        mapping = aes(x = x, y = y)),
        a.cipoly,
        inherit.aes = FALSE)
      )
  } 
  
  # Add partial residuals
  if (partial.resids &&
      (a.res$o$by.resids | x$by == "NA")) { 
    if (length(P$raw) == length(P$p.resid)) {
      .pl <- .pl + 
        do.call(geom_point,
                c(list(
                  data = .datRes, 
                  mapping = aes(x = resx, y = resy)),
                  na.rm = TRUE,
                  a.res$o$g)
        )
    } else {
      warning("Partial residuals do not have a natural x-axis location for linear functional terms")
    }
  }
  
  # Add lines for confidence bands
  if (se) {
    .pl <- .pl +
      do.call(geom_line, c(list(mapping = aes(x = x, y = uci)), a.cilin)) +
      do.call(geom_line, c(list(mapping = aes(x = x, y = lci)), a.cilin))
  }
  
  # Add rug
  if (rug) { 
    if (a.rug$o$jit) {
      .datRes$resx <- jitter(.datRes$resx)
    }
    .pl <- .pl +
      do.call(geom_rug, c(list(
        data = .datRes, 
        mapping = aes(x = resx),
        inherit.aes = FALSE),
        a.rug$g)
      )
  }
  
  # Add mean (or quantile) effect
  .pl <- .pl + geom_line()
  
  # add theme to plot, can be overriden later
  .pl <- .pl + theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  return(.pl)
} 



