
#' QQ plots for gam model residuals
#' 
#' @description Takes a fitted gam object produced by [mgcv::gam()] and produces QQ plots of its residuals
#' (conditional on the fitted model coefficients and scale parameter). If the model 
#' distributional assumptions are met then usually these plots should be close to a straight
#' line (although discrete data can yield marked random departures from this line).
#'
#' @param object, A fitted `gam` object as produced by [mgcv::gam()] (or a `glm` object).
#' @param rep, How many replicate datasets to generate to simulate quantiles of the residual
#'  distribution. 0 results in an efficient simulation free method for direct calculation,
#'   if this is possible for the object family.
#' @param level, If simulation is used for the quantiles, then reference intervals can be provided
#'  for the QQ-plot, this specifies the level. 0 or less for no intervals, 1 or more to simply plot
#'   the QQ plot for each replicate generated.
#' @param s.rep, How many times to randomize uniform quantiles to data under direct computation.
#' @param type, What sort of residuals should be plotted? See [mgcv::residuals.gam()].
#' @param rl.col, Color for the reference line on the plot.
#' @param rep.col, Color for reference bands or replicate reference plots.
#' @param ..., Extra parameters. 
#' @note Help file is mainly from [mgcv::qq.gam] since this is a rewrite of `mgcv::qq.gam` 
#' function with ggplot2 library.
#' @import ggplot2
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ## simulate binomial data...
#' library(mgcv)
#' library(mgcViz)
#' set.seed(0)
#' n.samp <- 400
#' dat <- gamSim(1,n=n.samp,dist="binary",scale=.33)
#' p <- binomial()$linkinv(dat$f) ## binomial p
#' n <- sample(c(1,3),n.samp,replace=TRUE) ## binomial n
#' dat$y <- rbinom(n,n,p)
#' dat$n <- n
#' lr.fit <- gam(y/n ~ s(x0) + s(x1) + s(x2) + s(x3)
#'               , family = binomial, data = dat,
#'               weights = n, method = "REML")
#' ## normal QQ-plot of deviance residuals
#' mgcViz::qqnorm(residuals(lr.fit))
#' ## Quick QQ-plot of deviance residuals
#' mgcv::qq.gam(lr.fit, pch = 19, cex = .3)
#' mgcViz::qq.gam(lr.fit)
#' ## Simulation based QQ-plot with reference bands 
#' mgcv::qq.gam(lr.fit, rep = 100, level = .9)
#' mgcViz::qq.gam(lr.fit, rep = 100, level = .9)
#' ## Simulation based QQ-plot, Pearson resids, all
#' ## simulated reference plots shown...  
#' mgcv::qq.gam(lr.fit, rep = 100, level = 1, type = "pearson", pch = 19, cex = .2)
#' mgcViz::qq.gam(lr.fit, rep = 100, level = 1, type = "pearson")
#' ## Now fit the wrong model and check....
#' pif <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3)
#'            , family = poisson, data = dat, method = "REML")
#' mgcViz::qqnorm(residuals(pif))
#' ##
#' mgcv::qq.gam(pif, pch = 19, cex = .3)
#' mgcViz::qq.gam(pif)
#' ##
#' mgcv::qq.gam(pif, rep = 100, level = .9)
#' mgcViz::qq.gam(pif, rep = 100, level = .9)
#' ##
#' mgcv::qq.gam(pif, rep = 100, level = 1, type = "pearson", pch = 19, cex = .2)
#' mgcViz::qq.gam(pif, rep = 100, level = 1, type = "pearson")
#' ## Example of binary data model violation so gross that you see a problem 
#' ## on the QQ plot...
#' y <- c(rep(1, 10), rep(0, 20), rep(1, 40), rep(0, 10), rep(1, 40), rep(0, 40))
#' x <- 1:160
#' b <- glm(y ~ x, family = binomial)
#' ## Note that the next two are not necessarily similar under gross 
#' ## model violation...
#' mgcv::qq.gam(b)
#' mgcViz::qq.gam(b)
#' mgcv::qq.gam(b, rep = 50, level = 1)
#' mgcViz::qq.gam(b, rep = 50, level = 1)
#' ## alternative model
#' b <- gam(y ~ s(x, k = 5), family = binomial, method = "ML")
#' mgcv::qq.gam(b)
#' mgcViz::qq.gam(b)
#' mgcv::qq.gam(b, rep = 50, level = 1)
#' mgcViz::qq.gam(b, rep = 50, level = 1)
#' }
qq.gam <- function (object, rep = 10,
                    level = 0.8, s.rep = 10,
                    type = c("deviance", "pearson", "response"),
                    rl.col = 2,
                    rep.col = "gray80", ...) {
  force(rep.col)
  type <- match.arg(type)
  ylab <- paste(type, "residuals")
  if (inherits(object, c("glm", "gam"))) {
    if (is.null(object$sig2)) 
      object$sig2 <- summary(object)$dispersion
  } else {
    stop("'object' is not a glm or gam.")
  }
  object$na.action <- NULL
  D <- residuals(object, type = type)
  if (object$method %in% c("PQL", "lme.ML", "lme.REML", "lmer.REML", 
                           "lmer.ML", "glmer.ML")) {
    p <- qqnorm(D, ylab = ylab, ...)
    return(p)
  }
  lim <- Dq <- NULL
  if (rep == 0) {
    fam <- mgcv::fix.family.qf(object$family)
    if (is.null(fam$qf)) 
      rep <- 50
    level <- 0
  }
  n <- length(D)
  if (rep > 0) {
    fam <- fix.family.rd(object$family)
    if (!is.null(fam$rd)) {
      dm <- matrix(0, n, rep)
      for (i in 1:rep) {
        yr <- fam$rd(object$fitted.values, object$prior.weights, 
                     object$sig2)
        object$y <- yr
        dm[, i] <- sort(residuals(object, type = type))
      }
      Dq <- quantile(as.numeric(dm), (1:n - 0.5)/n)
      alpha <- (1 - level)/2
      if (alpha > 0.5 || alpha < 0) 
        alpha <- 0.05
      if (level > 0 && level < 1) 
        lim <- apply(dm, 1, FUN = quantile,
                     p = c(alpha, 1 - alpha))
      else if (level >= 1) 
        lim <- level
    }
  } else {
    ix <- rank(D)
    U <- (ix - 0.5)/length(D)
    if (!is.null(fam$qf)) {
      dm <- matrix(0, n, s.rep)
      for (i in 1:s.rep) {
        U <- sample(U, n)
        q0 <- fam$qf(U, object$fitted.values, object$prior.weights, 
                     object$sig2)
        object$y <- q0
        dm[, i] <- sort(residuals(object, type = type))
      }
      Dq <- sort(rowMeans(dm))
    }
  }
  if (!is.null(Dq)) {
    p0 <- mgcViz::qqplot(Dq, D, ylab = ylab, xlab = "theoretical quantiles") 
    if (!is.null(lim)) {
      if (level >= 1) {
        dm <- data.frame(id = as.factor(rep(1:ncol(dm), each = nrow(dm))),
                         x = rep(Dq, ncol(dm)),
                         value = as.numeric(dm))
        p1 <- ggplot2::ggplot(dm, ggplot2::aes(x = x, y = value)) 
        p1 <- p1 + ggplot2::geom_line(aes(group = id), colour = rep.col)
      } else {
        dpoly <- data.frame(x = c(Dq, rev(Dq)), y = c(lim[1, ], rev(lim[2, ])))
        p1 <- ggplot2::ggplot(data = dpoly, ggplot2::aes(x = x, y = y)) + 
          ggplot2::geom_polygon(fill = rep.col)
      }
    } else {
      p1 <- ggplot()
    }
    p1 <- 
      p1 +
      ggplot2::geom_abline(colour = rl.col) +
      ggplot2::geom_point(data = p0$data, ggplot2::aes(x = sx, y = sy))
    return(p1)
  } else {
    return(qqnorm(D, ylab = ylab, ...))
  }
}

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
#' @param rep,level,rl.col,rep.col, Arguments passed to [qq.gam()].
#' @param ... Extra parameters. 
#' @return
#' @note Help file is mainly from [mgcv::gam.check] since this is a rewrite of `mgcv::gam.check` 
#' function with ggplot2 library.
#' @export
#' @examples
#' library(ggplot2)
#' set.seed(0)
#' dat <- mgcv::gamSim(1, n = 200)
#' b <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
#' mgcv::gam.check(b, pch = 19, cex = .3)
#' cg <- check.gam(b)
#' for (k in cg) print(k + theme_bw())
check.gam <- function(object,
                      type = c("deviance","pearson","response"),
                      k.sample = 5000,
                      k.rep = 200,
                      rep = 0, level = .9,
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
