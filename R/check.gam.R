#'
#' Some diagnostics for a fitted gam model
#' 
#' @description Takes a fitted gam object produced by [mgcv::gam()] and produces some diagnostic
#'  information about the fitting procedure and results. The default is to produce 4 residual plots,
#'   some information about the convergence of the smoothness selection optimization, and
#'    to run diagnostic tests of whether the basis dimension choises are adequate. 
#' @param object, A fitted `gam` object as produced by [mgcv::gam()].
#' @param type, Type of residuals, see [mgcv::residuals.gam()], used in all plots.
#' @param k.sample, Above this k testing uses a random sub-sample of data.
#' @param k.rep, How many re-shuffles to do to get p-value for k testing.
#' @param rep,level,method,rl.col,rep.col, Arguments passed to [qq.gam()].
#' @param ... Extra parameters. 
#' @return
#' @note Help file is mainly from [mgcv::gam.check] since this is a rewrite of `mgcv::gam.check` 
#' function with ggplot2 library.
#' @export check.gam
#' @examples
#' library(ggplot2)
#' set.seed(0)
#' dat <- mgcv::gamSim(1, n = 200)
#' b <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
#' mgcv::gam.check(b, pch = 19, cex = .3)
#' cg <- check(b) # Calls mgcViz::check.gam
#' for (k in cg) print(k + theme_bw())
check.gam <- function(object,
                      type = c("deviance","pearson","response"),
                      k.sample = 5000,
                      k.rep = 200,
                      rep = 10, level = .9, method = "simul2",
                      rl.col = 2, rep.col = "gray80", ...){
  
  type <- match.arg(type)
  resid <- residuals(b, type = type)
  linpred <- if (is.matrix(b$linear.predictors) && !is.matrix(resid)) { 
    napredict(b$na.action, b$linear.predictors[, 1])
  } else {
    napredict(b$na.action, b$linear.predictors)
  } 
  fv <- if (inherits(b$family, "extended.family")) {
    predict(b, type = "response")
  } else {
    fitted(b)
  }
  if (is.matrix(fv) && !is.matrix(b$y)) {
    fv <- fv[, 1]
  }
  resp <- napredict(b$na.action, b$y)
  df <- data.frame(linpred = linpred, resid = resid,
                   response = resp, fv = fv)
  plots <- list()
  plots[[1]] <- 
    mgcViz::qq.gam(b, rep = rep,
                   level = level, type = type, rl.col = rl.col, 
                   rep.col = rep.col)
  plots[[2]] <- 
    ggplot2::ggplot(data = df, aes(x = linpred, y = resid)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Resids vs. linear pred.",
                  x = "linear predictor", y = "residuals")
  plots[[3]] <- 
    ggplot2::ggplot() +
    ggplot2::geom_histogram(data = df, ggplot2::aes(x = resid)) +
    ggplot2::labs(title = "Histogram of residuals",
                  xlab = "Residuals")
  plots[[4]] <- 
    ggplot2::ggplot(data = df, aes(x = fv, y = resp)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Response vs. Fitted Values",
                  x = "Fitted Values", y = "Response")
  if (!(b$method %in% c("GCV", "GACV", "UBRE", "REML", "ML", 
                        "P-ML", "P-REML", "fREML"))) {
    return(plots)
  }
  cat("\nMethod:", b$method, "  Optimizer:", b$optimizer)
  if (!is.null(b$outer.info)) {
    if (b$optimizer[2] %in% c("newton", "bfgs")) {
      boi <- b$outer.info
      cat("\n", boi$conv, " after ", boi$iter, " iteration", 
          sep = "")
      if (boi$iter == 1) 
        cat(".")
      else cat("s.")
      cat("\nGradient range [", min(boi$grad), ",", max(boi$grad), 
          "]", sep = "")
      cat("\n(score ", b$gcv.ubre, " & scale ", b$sig2, 
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
      print(b$outer.info)
    }
  }
  else {
    if (length(b$sp) == 0) 
      cat("\nModel required no smoothing parameter selection")
    else {
      cat("\nSmoothing parameter selection converged after", 
          b$mgcv.conv$iter, "iteration")
      if (b$mgcv.conv$iter > 1) 
        cat("s")
      if (!b$mgcv.conv$fully.converged) 
        cat(" by steepest\ndescent step failure.\n")
      else cat(".\n")
      cat("The RMS", b$method, "score gradient at convergence was", 
          b$mgcv.conv$rms.grad, ".\n")
      if (b$mgcv.conv$hess.pos.def) 
        cat("The Hessian was positive definite.\n")
      else cat("The Hessian was not positive definite.\n")
    }
  }
  if (!is.null(b$rank)) {
    cat("Model rank = ", b$rank, "/", length(b$coefficients), 
        "\n")
  }
  cat("\n")
  kchck <- mgcv:::k.check(b, subsample = k.sample, n.rep = k.rep)
  if (!is.null(kchck)) {
    cat("Basis dimension (k) checking results. Low p-value (k-index<1) may\n")
    cat("indicate that k is too low, especially if edf is close to k'.\n\n")
    printCoefmat(kchck, digits = 3)
  }
  return(plots)
}
