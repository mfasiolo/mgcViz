#'
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
#' @importFrom data.table frankv
#' @importFrom matrixStats rowSds rowOrderStats
#' @return
#' @export
#' @examples
#' ## simulate binomial data...
#' library(mgcv)
#' library(mgcViz)
#' set.seed(0)
#' n.samp <- 400
#' dat <- gamSim(1,n = n.samp, dist = "binary", scale = .33)
#' p <- binomial()$linkinv(dat$f) ## binomial p
#' n <- sample(c(1, 3), n.samp, replace = TRUE) ## binomial n
#' dat$y <- rbinom(n, n, p)
#' dat$n <- n
#' lr.fit <- gam(y/n ~ s(x0) + s(x1) + s(x2) + s(x3)
#'               , family = binomial, data = dat,
#'               weights = n, method = "REML")
#' ## normal QQ-plot of deviance residuals
#' mgcViz::qqnorm(residuals(lr.fit))
#' ## Quick QQ-plot of deviance residuals
#' mgcv::qq.gam(lr.fit, pch = 19, cex = .3)
#' mgcViz::qq.gam(lr.fit, method = "simul2")
#' ## Simulation based QQ-plot with reference bands 
#' mgcv::qq.gam(lr.fit, rep = 100, level = .9)
#' mgcViz::qq.gam(lr.fit, rep = 100, level = .9, CI = "quantile")
#' ## Simulation based QQ-plot, Pearson resids, all
#' ## simulated reference plots shown...  
#' mgcv::qq.gam(lr.fit, rep = 100, level = 1, type = "pearson", pch = 19, cex = .2)
#' mgcViz::qq.gam(lr.fit, rep = 100, CI = "none", show.reps = TRUE, type = "pearson", shape=19)
#' ## Now fit the wrong model and check....
#' pif <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3)
#'            , family = poisson, data = dat, method = "REML")
#' mgcViz::qqnorm(residuals(pif))
#' ##
#' mgcv::qq.gam(pif, pch = 19, cex = .3)
#' mgcViz::qq.gam(pif, method = "simul2")
#' ##
#' mgcv::qq.gam(pif, rep = 100, level = .9)
#' mgcViz::qq.gam(pif, rep = 100, level = .9, CI = "quantile")
#' ##
#' mgcv::qq.gam(pif, rep = 100, level = 1, type = "pearson", pch = 19, cex = .2)
#' mgcViz::qq.gam(pif, rep = 100, type = "pearson", 
#'                CI = "none", show.reps = TRUE, shape = 19)
#' ## Example of binary data model violation so gross that you see a problem 
#' ## on the QQ plot...
#' y <- c(rep(1, 10), rep(0, 20), rep(1, 40), rep(0, 10), rep(1, 40), rep(0, 40))
#' x <- 1:160
#' b <- glm(y ~ x, family = binomial)
#' ## Note that the next two are not necessarily similar under gross 
#' ## model violation...
#' mgcv::qq.gam(b)
#' mgcViz::qq.gam(b, method = "simul2")
#' mgcv::qq.gam(b, rep = 50, level = 1)
#' mgcViz::qq.gam(b, rep = 50, CI = "none", show.reps = TRUE)
#' ## alternative model
#' b <- gam(y ~ s(x, k = 5), family = binomial, method = "ML")
#' mgcv::qq.gam(b)
#' mgcViz::qq.gam(b, method = "simul2")
#' mgcv::qq.gam(b, rep = 50, level = 1, pch = 19, cex = .2)
#' mgcViz::qq.gam(b, rep = 50, show.reps = T, CI = "none", shape = 19)
#' 
#' \dontrun{
#' # A "Big Data" example: 
#' set.seed(0)
#' n.samp <- 50000
#' dat <- gamSim(1,n=n.samp,dist="binary",scale=.33)
#' p <- binomial()$linkinv(dat$f) ## binomial p
#' n <- sample(c(1,3),n.samp,replace=TRUE) ## binomial n
#' dat$y <- rbinom(n,n,p)
#' dat$n <- n
#' lr.fit <- bam(y/n ~ s(x0) + s(x1) + s(x2) + s(x3)
#'               , family = binomial, data = dat,
#'               weights = n, method = "fREML", discrete = T)
#' 
#' # Turning discretization off (on by default for large datasets).
#' set.seed(414) # Setting the seed because qq.gam is doing simulations
#' o <- qq.gam(lr.fit, rep = 10, method = "simul1", CI = "normal", show.reps = T, rep.alpha = 0.1, 
#'             discrete = F)
#' o # This might take some time!
#' 
#' # Using default discretization
#' set.seed(414)
#' o <- qq.gam(lr.fit, rep = 10, method = "simul1", CI = "normal", show.reps = T, rep.alpha = 0.1)
#' o # Much faster plotting!
#' 
#' # Very coarse discretization
#' set.seed(414)
#' o <- qq.gam(lr.fit, rep = 10, method = "simul1", CI = "normal", show.reps = T, rep.alpha = 0.1, 
#'             discrete = TRUE, ngr = 1e2, shape = 19)
#' o 
#' 
#' # We can also zoom in at now extra costs (most work already done by qq.gam)
#' zoom(o, xlim = c(-0.25, 0.25), show.reps = T, rep.alpha = 0.1, discrete = T)
#'
#'}
qq.gam <- function(object, rep = 10,
                    level = 0.8, 
                    method = c("simul1", "simul2", "tnormal", "tunif", "normal"),
                    type = c("deviance", "pearson", "response"),
                    CI = c("normal", "quantile", "none"),
                    show.reps = FALSE,
                    sortFun = NULL,
                    discrete = NULL,
                    ngr = 1e3,
                    rl.col = 2,
                    rep.col = 1,
                    rep.alpha = 0.05,
                    ci.col = "gray80",
                    shape = '.', ...) {
  
  CI     <- match.arg(CI, c("normal", "quantile", "none"))
  method <- match.arg(method, c("simul1", "simul2", "tnormal", "tunif", "normal"))
  type   <- match.arg(type, c("deviance", "pearson", "response"))
  if (level < 0 || level > 1){
    stop("`level' should be between 0 and 1") 
  }
  if (method == "simul2") CI <- "none"
  if (is.null(sortFun))  sortFun  <- function(.x) sort(.x, method = "quick")
  if (is.null(discrete)) discrete <- length(object$y) > 1e4
  # force(rep.col)
  if (inherits(object, c("glm", "gam"))) {
    if (is.null(object$sig2)) 
      object$sig2 <- summary(object)$dispersion
  } else {
    stop("'object' is not a glm or gam.")
  }
  object$na.action <- NULL
  P0 <- .compute.qq.gam(o = object, type = type, method = method, CI = CI, 
                       level = level, rep = rep, sortFun = sortFun)
  P1 <- .discretize.qq.gam(P = P0, discrete = discrete, ngr = ngr,
                           CI = (CI != "none"), show.reps = show.reps)
  pl <- .plot.qq.gam(P = P1, CI = (CI != "none"), show.reps = show.reps,
                     rl.col = rl.col, rep.col = rep.col, 
                     rep.alpha = rep.alpha, ci.col = ci.col, shape = shape)
  out <- structure(list("ggPlot" = pl, "store" = P0),
                   "class" = "qqGam", 
                   "call" = match.call())
  return(out)
}


