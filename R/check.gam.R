#'
#' Some diagnostics for a fitted gam model
#' 
#' @description Takes a fitted gam object produced by [mgcv::gam()] and produces some diagnostic
#'  information about the fitting procedure and results. The default is to produce 4 residual plots,
#'   some information about the convergence of the smoothness selection optimization, and
#'    to run diagnostic tests of whether the basis dimension choises are adequate. 
#' @param o, A fitted `gam` object as produced by [mgcv::gam()].
#' @param type, Type of residuals, see [mgcv::residuals.gam()], used in all plots.
#' @param k.sample, Above this k testing uses a random sub-sample of data.
#' @param k.rep, How many re-shuffles to do to get p-value for k testing.
#' @param rep,level,method,rl.col,rep.col, Arguments passed to [qq.gam()].
#' @param ... Extra parameters. 
#' @return An object of class \code{check.gam}, which is simply a list of \code{ggplot} objects.
#' @note Help file is mainly from [mgcv::gam.check] since this is a rewrite of `mgcv::gam.check` 
#' function with ggplot2 library.
#' @importFrom stats napredict fitted printCoefmat 
#' @examples
#' library(ggplot2)
#' set.seed(0)
#' dat <- gamSim(1, n = 200)
#' b <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
#' 
#' # Checks using default options
#' check(b)
#' 
#' # Change some algorithmic and graphical parameters
#' check(b,
#'       a.qq = list(method = "tnorm", 
#'                   a.cipoly = list(fill = "light blue")), 
#'       a.respoi = list(size = 0.5), 
#'       a.hist = list(bins = 10))
#' @export check.gam
#' 
check.gam <- function(o,
                      type = c("auto", "deviance", "pearson", "response", "tunif", "tnormal"),
                      k.sample = 5000,
                      k.rep = 200,
                      maxpo = 1e4,
                      a.qq = list(),
                      a.hist = list(),
                      a.respoi = list(),
                      a.reshex = list(),
                      ...){
  
  type <- match.arg(type)
  if (type == "auto") { type <- .getResTypeAndMethod(o$family$family)$type }
  
  # Overwriting user-provided argument lists
  a.all <- .argMaster("check.gam")
  for(nam in names(a.all)){
    assign(nam, .argSetup(a.all[[nam]], get(nam), nam, verbose = FALSE), envir = environment())
  }
  
  resid <- residuals(o, type = type)
 
  # Sample if too many points (> maxpo) 
  nres <- length( resid )
  subS <- if(nres > maxpo) { 
    sample( c(rep(T, maxpo), rep(F, nres-maxpo)) )
  } else { 
    rep(T, nres) 
  }  
  
  linpred <- if (is.matrix(o$linear.predictors) && !is.matrix(resid)) { 
    napredict(o$na.action, o$linear.predictors[, 1])
  } else {
    napredict(o$na.action, o$linear.predictors)
  } 
  fv <- if (inherits(o$family, "extended.family")) {
    predict(o, type = "response")
  } else {
    fitted(o)
  }
  if (is.matrix(fv) && !is.matrix(o$y)) {
    fv <- fv[, 1]
  }
  resp <- napredict(o$na.action, o$y)
  df <- data.frame(linpred = linpred, resid = resid,
                   response = resp, fv = fv)
  dfS <- df[subS, ]
  
  plots <- list()
  plots[[1]] <- 
    do.call("qq.gam", c(list("o" = o), a.qq))$ggObj
  plots[[2]] <- 
    ggplot(data = dfS, aes(x = linpred, y = resid)) +
    do.call("geom_point", a.respoi) +
    labs(title = "Resids vs. linear pred.",
                  x = "linear predictor", y = "residuals")
  plots[[3]] <- 
    ggplot(data = df, mapping = aes(x = resid)) +
    do.call("geom_histogram", a.hist)  +
    labs(title = "Histogram of residuals",
                  xlab = "Residuals")
  plots[[4]] <- 
    ggplot(data = dfS, aes(x = fv, y = response)) +
    do.call("geom_point", a.respoi) +
    labs(title = "Response vs. Fitted Values",
                  x = "Fitted Values", y = "Response")
  
  if ( (o$method %in% c("GCV", "GACV", "UBRE", "REML", "ML",  "P-ML", "P-REML", "fREML")) ) {
    cat("\nMethod:", o$method, "  Optimizer:", o$optimizer)
    if (!is.null(o$outer.info)) {
      if (o$optimizer[2] %in% c("newton", "bfgs")) {
        boi <- o$outer.info
        cat("\n", boi$conv, " after ", boi$iter, " iteration", 
            sep = "")
        if (boi$iter == 1) 
          cat(".")
        else cat("s.")
        cat("\nGradient range [", min(boi$grad), ",", max(boi$grad), 
            "]", sep = "")
        cat("\n(score ", o$gcv.ubre, " & scale ", o$sig2, 
            ").", sep = "")
        ev <- eigen(boi$hess)$values
        if (min(ev) > 0) 
          cat("\nHessian positive definite, ")
        else cat("\n")
        cat("eigenvalue range [", min(ev), ",", max(ev), 
            "].\n", sep = "")
      }
      else {
        cat("\n")
        print(o$outer.info)
      }
    }
    else {
      if (length(o$sp) == 0) 
        cat("\nModel required no smoothing parameter selection")
      else {
        cat("\nSmoothing parameter selection converged after", 
            o$mgcv.conv$iter, "iteration")
        if (o$mgcv.conv$iter > 1) 
          cat("s")
        if (!o$mgcv.conv$fully.converged) 
          cat(" by steepest\ndescent step failure.\n")
        else cat(".\n")
        cat("The RMS", o$method, "score gradient at convergence was", 
            o$mgcv.conv$rms.grad, ".\n")
        if (o$mgcv.conv$hess.pos.def) 
          cat("The Hessian was positive definite.\n")
        else cat("The Hessian was not positive definite.\n")
      }
    }
    if (!is.null(o$rank)) {
      cat("Model rank = ", o$rank, "/", length(o$coefficients), 
          "\n")
    }
    cat("\n")
    kchck <- mgcv:::k.check(o, subsample = k.sample, n.rep = k.rep)
    if (!is.null(kchck)) {
      cat("Basis dimension (k) checking results. Low p-value (k-index<1) may\n")
      cat("indicate that k is too low, especially if edf is close to k'.\n\n")
      printCoefmat(kchck, digits = 3)
    }
  }
  
  class(plots) <- "checkGam"
  
  return(plots)
}
