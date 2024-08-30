#'
#' Some diagnostics for a fitted gam model
#' 
#' @description Takes a fitted GAM model and produces some diagnostic information about the fitting
#'              procedure and results. The default is to produce 4 residual plots, some information about
#'              the convergence of the smoothness selection optimization, and to run diagnostic tests of 
#'              whether the basis dimension choises are adequate. 
#' @param obj an object of class \code{gamViz}, the output of a \code{getViz()} call.
#' @param type type of residuals, see [residuals.gamViz], used in all plots.
#' @param k.sample above this k testing uses a random sub-sample of data.
#' @param k.rep how many re-shuffles to do to get p-value for k testing.
#' @param maxpo maximum number of residuals points that will be plotted in the scatter-plots.
#'              If number of datapoints > \code{maxpo}, then a subsample of \code{maxpo} points will be plotted.
#' @param a.qq list of arguments to be passed to \code{qq.gamViz}. See [qq.gamViz]. 
#' @param a.hist list of arguments to be passed to \code{ggplot2::geom_histogram}. 
#' @param a.respoi list of arguments to be passed to \code{ggplot2::geom_point}. 
#' @param ... currently not used. 
#' @details This is a essentially a re-write of \code{mgcv::gam.check} using \code{ggplot2}. See 
#'          [mgcv::gam.check] for details. 
#' @return An object of class \code{checkGam}, which is simply a list of \code{ggplot} objects.
#' @importFrom stats napredict fitted printCoefmat 
#' @importFrom qgam check check.qgam
#' @importFrom mgcv k.check
#' @examples
#' library(mgcViz)
#' set.seed(0)
#' dat <- gamSim(1, n = 200)
#' b <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
#' b <- getViz(b)
#' 
#' # Checks using default options
#' check(b)
#' 
#' # Change some algorithmic and graphical parameters
#' check(b,
#'       a.qq = list(method = "tnorm", 
#'                   a.cipoly = list(fill = "light blue")), 
#'       a.respoi = list(size = 0.2), 
#'       a.hist = list(bins = 10))
#' @export check.gamViz
#' @export
#' 
check.gamViz <- function(obj,
                         type = c("auto", "deviance", "pearson", "response", "tunif", "tnormal"),
                         k.sample = 5000,
                         k.rep = 200,
                         maxpo = 1e4,
                         a.qq = list(),
                         a.hist = list(),
                         a.respoi = list(),
                         ...){
  
  if( !inherits(obj, "gamViz") ){ stop("Argument 'obj' should be of class 'gamViz'. See ?getViz") }
  
  if( inherits(obj, "qgam") ){ return( check.qgam(obj) ) }
  
  type <- match.arg(type)
  if (type == "auto") { type <- .getResTypeAndMethod(obj$family)$type }
  
  # Overwriting user-provided argument lists
  a.all <- .argMaster("check.gamViz")
  for(nam in names(a.all)){
    assign(nam, .argSetup(a.all[[nam]], get(nam), nam, verbose = FALSE), envir = environment())
  }
  
  if(is.null(a.qq$type)){
    a.qq$type <- type
  }
  
  resid <- residuals(obj, type = type)
 
  # Sample if too many points (> maxpo) 
  nres <- length( resid )
  subS <- if(nres > maxpo) { 
    sample( c(rep(T, maxpo), rep(F, nres-maxpo)) )
  } else { 
    rep(T, nres) 
  }  
  
  linpred <- if (is.matrix(obj$linear.predictors) && !is.matrix(resid)) { 
    napredict(obj$na.action, obj$linear.predictors[, 1])
  } else {
    napredict(obj$na.action, obj$linear.predictors)
  } 
  fv <- if (inherits(obj$family, "extended.family")) {
    predict(obj, type = "response")
  } else {
    fitted(obj)
  }
  if (is.matrix(fv) && !is.matrix(obj$y)) {
    fv <- fv[, 1]
  }
  resp <- napredict(obj$na.action, obj$y)
  df <- data.frame(linpred = linpred, resid = resid,
                   response = resp, fv = fv)
  dfS <- df[subS, ]
  
  plots <- list()
  plots[[1]] <- 
    do.call("qq.gamViz", c(list("o" = obj), a.qq))$ggObj
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
  
  if ( (obj$method %in% c("GCV", "GACV", "UBRE", "REML", "ML",  "P-ML", "P-REML", "fREML")) ) {
    cat("\nMethod:", obj$method, "  Optimizer:", obj$optimizer)
    if (!is.null(obj$outer.info)) {
      if (obj$optimizer[2] %in% c("newton", "bfgs")) {
        boi <- obj$outer.info
        cat("\n", boi$conv, " after ", boi$iter, " iteration", 
            sep = "")
        if (boi$iter == 1) 
          cat(".")
        else cat("s.")
        if( is.null(obj$family$available.derivs) || obj$family$available.derivs > 0 ){
        cat("\nGradient range [", min(boi$grad), ",", max(boi$grad), 
            "]", sep = "")
        cat("\n(score ", obj$gcv.ubre, " & scale ", obj$sig2, 
            ").", sep = "")
        ev <- eigen(boi$hess)$values
        if (min(ev) > 0) 
          cat("\nHessian positive definite, ")
        else cat("\n")
        cat("eigenvalue range [", min(ev), ",", max(ev), 
            "].\n", sep = "")
        }
      }
      else {
        cat("\n")
        print(obj$outer.info)
      }
    }
    else {
      if (length(obj$sp) == 0) 
        cat("\nModel required no smoothing parameter selection")
      else {
        if( !is.null(obj$mgcv.conv) ){
        cat("\nSmoothing parameter selection converged after", 
            obj$mgcv.conv$iter, "iteration")
        if (obj$mgcv.conv$iter > 1) 
          cat("s")
        if (!obj$mgcv.conv$fully.converged) 
          cat(" by steepest\ndescent step failure.\n")
        else cat(".\n")
        cat("The RMS", obj$method, "score gradient at convergence was", 
            obj$mgcv.conv$rms.grad, ".\n")
        if (obj$mgcv.conv$hess.pos.def) 
          cat("The Hessian was positive definite.\n")
        else cat("The Hessian was not positive definite.\n")
        }
      }
    }
    if (!is.null(obj$rank)) {
      cat("Model rank = ", obj$rank, "/", length(obj$coefficients), 
          "\n")
    }
    cat("\n")
    kchck <- k.check(obj, subsample = k.sample, n.rep = k.rep)
    if (!is.null(kchck)) {
      cat("Basis dimension (k) checking results. Low p-value (k-index<1) may\n")
      cat("indicate that k is too low, especially if edf is close to k'.\n\n")
      printCoefmat(kchck, digits = 3)
    }
  }
  
  class(plots) <- "checkGam"
  
  return(plots)
}
