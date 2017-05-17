# code mostly from mgcv:: and stats:: (qqplot, qqnorm, qq.gam, gam.check)
# TODO : layer in right order : points above and curves behind. Proper labels, and
# ability for the user to pass graphical options. Also improve multiple lines
# plotting to it in one pass
# Also : change output values. returns the object ? the old values and just print the plot ?
# Available as an option ??
# In plotly subplot, arrange titles, etc
# rename <function>new in <function> in package and use namespaces for selecting the right
# plot between mgcv and mgcvextra ? e.g. mgcv::qq.gam and mgcvextra::qq.gam ?

#' qqnorm
#' 
#' @param y, 
#' @param ylim, 
#' @param main,
#' @param xlab, 
#' @param ylab, 
#' @param datax, Logical. Should data values be on the x-axis
#' @import ggplot2
#' @export
#' @examples 
#' y <- rt(500, df = 5)
#' mgcViz::qqnorm(y)
#' stats::qqnorm(y)
#' mgcViz::qqnorm(precip, ylim = c(0,50),
#'  ylab = "Precipitation [in/yr] for 70 US cities") + theme_minimal()
#' stats::qqnorm(precip, ylab = "Precipitation [in/yr] for 70 US cities")
#' plotly::ggplotly(mgcViz::qqnorm(y) + theme_minimal())
qqnorm <- function(y, ylim,
                   main = "Normal Q-Q Plot",
                   xlab = "Theoretical Quantiles", 
                   ylab = "Sample Quantiles",
                   datax = FALSE) 
{
  if (has.na <- any(ina <- is.na(y))) {
    yN <- y
    y <- y[!ina]
  }
  if (0 == (n <- length(y))) 
    stop("y is empty or has only NAs")
  if (missing(ylim)) 
    ylim <- range(y)
  x <- qnorm(ppoints(n))[order(order(y))]
  if (has.na) {
    y <- x
    x <- yN
    x[!ina] <- y
    y <- yN
  }
  xtitle <- ifelse(datax, ylab, xlab)
  ytitle <- ifelse(datax, xlab, ylab)
  data <- data.frame(x = x, y = y)
  if (datax) {
    p <- ggplot2::ggplot(data, aes(x = y, y = x)) + ggplot2::geom_point()
  } else {
    p <- ggplot2::ggplot(data, aes(x = x, y = y)) + ggplot2::geom_point()
  }
  p <- p + ylim(ylim) + ggplot2::labs(title = main, x = xlab, y = ylab)
  return(p)
}

#' qqplot
#' 
#' @param x, 
#' @param y, 
#' @param xlab, 
#' @param ylab, 
#' @param main,
#' @import ggplot2
#' @examples 
#' x <- rt(200, df = 5)
#' y <- rt(300, df = 5)
#' stats::qqplot(x, y)
#' mgcViz::qqplot(x, y)
#' ## "QQ-Chisquare" : --------------------------
#' y <- rchisq(500, df = 3)
#' ## Q-Q plot for Chi^2 data against true theoretical distribution:
#' x <- qchisq(ppoints(500), df = 3)
#' stats::qqplot(qchisq(ppoints(500), df = 3), rchisq(500, df = 3),
#'       main = expression("Q-Q plot for" ~~ {chi^2}[nu == 3]))
#' p <- mgcViz::qqplot(qchisq(ppoints(500), df = 3), rchisq(500, df = 3),
#'          main = expression("Q-Q plot for" ~~ {chi^2}[nu == 3])) + theme_bw()
#' p          
#' plotly::ggplotly(p + theme_minimal()) # title not working
qqplot <- function(x, y,
                   xlab = deparse(substitute(x)), 
                   ylab = deparse(substitute(y)),
                   main = "Q-Q Plot"){
  sx <- sort(x)
  sy <- sort(y)
  lenx <- length(sx)
  leny <- length(sy)
  if (leny < lenx) 
    sx <- approx(1L:lenx, sx, n = leny)$y
  if (leny > lenx) 
    sy <- approx(1L:leny, sy, n = lenx)$y
  data <- data.frame(sx = sx, sy = sy)
  p <- ggplot2::ggplot(data, aes(x = sx, y = sy)) + ggplot2::geom_point()
  p <- p + ggplot2::labs(title = main, x = xlab, y = ylab)
  return(p)
}

#' qq.gam
#'
#' @param object, 
#' @param rep, 
#' @param level, 
#' @param s.rep, 
#' @param type, 
#' @param rl.col, 
#' @param rep.col, 
#' @param ... 
#' @import ggplot2
#' @return
#' @export
#'
#' @examples
#' ## simulate binomial data...
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
#' stats::qqnorm(residuals(lr.fit), pch = 19, cex = .3)
#' stats::qqnorm(residuals(lr.fit))
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
#' qqnorm(residuals(pif), pch = 19, cex = .3)
#' qqnorm(residuals(pif))
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
qq.gam <- function (object, rep = 0,
                    level = 0.9, s.rep = 10,
                    type = c("deviance", "pearson", "response"),
                    rl.col = 2,
                    rep.col = "gray80", ...) 
{
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
    p <- qqnormnew(D, ylab = ylab, ...)
    return(p)
  }
  lim <- Dq <- NULL
  if (rep == 0) {
    fam <- fix.family.qf(object$family)
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
        lim <- apply(dm, 1, FUN = quantile, p = c(alpha, 
                                                  1 - alpha))
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
    p0 <- qqplot(Dq, D, ylab = ylab, xlab = "theoretical quantiles") 
    p1 <- ggplot() 
    if (!is.null(lim)) {
      if (level >= 1) {
        dm2 <- lapply(1:ncol(dm), function(i) {
          data.frame(id = i, value = dm[, i])
        })
        dm2 <- Reduce("rbind", dm2)
        
        for (i in 1:rep) p <- add_trace(p, x = Dq, y = dm[, i], mode = "lines",
                                        line = list(color = 'rgba(204, 204, 204, 0.5)',
                                                    width = 0.5))
      }  else {
        n <- length(Dq)
        p <- add_ribbons(p,
                         x = Dq, 
                         ymin = lim[1, ], ymax = lim[2, ],
                         color = I("grey80"))
      }
    }
    p1 <- 
      p1 +
      geom_abline(colour = "red", size = 1.2) +
      geom_point(data = p0$data, aes(x = sx, y = sy))
    return(p)
  }
  else qqnormnew(D, ylab = ylab, ...)
}

#' gam.checknew
#'
#' @param b, 
#' @param type, 
#' @param k.sample,
#' @param k.rep, 
#' @param rep, 
#' @param level, 
#' @param rl.col, 
#' @param rep.col, 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(0)
#' dat <- gamSim(1,n=200)
#' b <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),data = dat)
#' gam.check(b, pch = 19, cex = .3)
#' gam.checknew(b)
gam.checknew <- function(b,
                         type = c("deviance","pearson","response"),
                         k.sample = 5000,
                         k.rep = 200,
                         rep = 0, level = .9,
                         rl.col = 2, rep.col = "gray80", ...){
  
  type <- match.arg(type)
  resid <- residuals(b, type = type)
  linpred <- if (is.matrix(b$linear.predictors) && !is.matrix(resid)) 
    napredict(b$na.action, b$linear.predictors[, 1])
  else napredict(b$na.action, b$linear.predictors)
  # if (is.null(.Platform$GUI) || .Platform$GUI != "RStudio") 
  #   old.par <- par(mfrow = c(2, 2))
  # if (old.style) 
  #   qqnorm(resid, ...)
  # else
  plots <- list()
  plots[[1]] <- qq.gamnew(b, rep = rep, level = level, type = type, rl.col = rl.col, 
                          rep.col = rep.col, ...)
  plots[[2]] <- plot_ly(x = ~linpred, y = ~resid, type = "scatter", mode = "markers") %>%
    layout(title = "Resids vs. linear pred.",
           xaxis = list(title = "linear predictor",
                        zeroline = FALSE,
                        showline = TRUE,
                        mirror = "ticks"),
           yaxis = list(title = "residuals",
                        zeroline = FALSE,
                        showline = TRUE,
                        mirror = "ticks"))
  plots[[3]] <- plot_ly(x = ~resid, type = "histogram") %>% 
    layout(title = "Histogram of residuals",
           xaxis = list(title = "Residuals"),
           yaxis = list(title = "Frequency"))
  fv <- if (inherits(b$family, "extended.family")) 
    predict(b, type = "response")
  else fitted(b)
  if (is.matrix(fv) && !is.matrix(b$y)) 
    fv <- fv[, 1]
  plots[[4]] <- plot_ly(x = ~fv, y = ~ napredict(b$na.action, b$y),
                        type = "scatter", mode = "markers") %>% 
    layout(title = "Response vs. Fitted Values",
           xaxis = list(title = "Fitted Values",
                        zeroline = FALSE,
                        showline = TRUE,
                        mirror = "ticks"),
           yaxis = list(title = "Response",
                        zeroline = FALSE,
                        showline = TRUE,
                        mirror = "ticks"))
  if (is.null(.Platform$GUI) || .Platform$GUI != "RStudio"){
    subplot(plots, nrows = 2, margin = 0.05,
            titleX = TRUE, titleY = TRUE, shareX = FALSE, shareY = FALSE)
  } else {
    for (k in plots) print(k)
  }
  if (!(b$method %in% c("GCV", "GACV", "UBRE", "REML", "ML", 
                        "P-ML", "P-REML", "fREML"))) {
    return(invisible())
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
}
