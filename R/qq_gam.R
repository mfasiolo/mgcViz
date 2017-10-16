#'
#' QQ plots for gam model residuals
#' 
#' @description Takes a fitted gam object produced by [mgcv::gam()] and produces QQ plots of its residuals
#' (conditional on the fitted model coefficients and scale parameter). If the model 
#' distributional assumptions are met then usually these plots should be close to a straight
#' line (although discrete data can yield marked random departures from this line).
#'
#' @param o, A fitted `gam` object as produced by [mgcv::gam()] (or a `glm` object).
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
#' @importFrom stats residuals sd qnorm dnorm qbeta approx var predict
#' @importFrom mgcv fix.family.qf fix.family.rd
#' @importFrom data.table frankv
#' @importFrom matrixStats rowSds rowOrderStats
#' @return An object of class \code{qqGam}.
#' @export
#' @examples
#'  
#' ######## Example: simulate binomial data
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
#' 
#' # Quick QQ-plot of deviance residuals
#' qq.gam(lr.fit, method = "simul2")
#' 
#' # Same, but changing points share and type of reference list
#' qq.gam(lr.fit, method = "simul2", 
#'        a.qqpoi = list("shape" = 1), a.ablin = list("linetype" = 2))
#' 
#' # Simulation based QQ-plot with reference bands 
#' qq.gam(lr.fit, rep = 100, level = .9, CI = "quantile")
#' 
#' # Simulation based QQ-plot, Pearson resids, all simulations lines shown 
#' qq.gam(lr.fit, rep = 100, CI = "none", show.reps = TRUE, type = "pearson", 
#'        a.qqpoi = list(shape=19, size = 0.5))
#' 
#' ### Now fit the wrong model and check
#' pif <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3)
#'            , family = poisson, data = dat, method = "REML")
#' 
#' #
#' qq.gam(pif, method = "simul2")
#' 
#' #
#' qq.gam(pif, rep = 100, level = .9, CI = "quantile")
#' 
#' #
#' mgcViz::qq.gam(pif, rep = 100, type = "pearson", CI = "none", show.reps = TRUE, 
#'                a.qqpoi = list(shape=19, size = 0.5))
#' 
#' ######## Example: binary data model violation so gross that you see a problem 
#' ######## on the QQ plot
#' y <- c(rep(1, 10), rep(0, 20), rep(1, 40), rep(0, 10), rep(1, 40), rep(0, 40))
#' x <- 1:160
#' b <- glm(y ~ x, family = binomial)
#' 
#' # Note that the next two are not necessarily similar under gross 
#' # model violation...
#' qq.gam(b, method = "simul2")
#' qq.gam(b, rep = 50, CI = "none", show.reps = TRUE)
#' 
#' ### alternative model
#' b <- gam(y ~ s(x, k = 5), family = binomial, method = "ML")
#' 
#' mgcViz::qq.gam(b, method = "simul2")
#' mgcViz::qq.gam(b, rep = 50, show.reps = TRUE, CI = "none", shape = 19)
#' 
#' \dontrun{
#' ########  "Big Data" example: 
#' set.seed(0)
#' n.samp <- 50000
#' dat <- gamSim(1,n=n.samp,dist="binary",scale=.33)
#' p <- binomial()$linkinv(dat$f) ## binomial p
#' n <- sample(c(1,3),n.samp,replace=TRUE) ## binomial n
#' dat$y <- rbinom(n,n,p)
#' dat$n <- n
#' lr.fit <- bam(y/n ~ s(x0) + s(x1) + s(x2) + s(x3)
#'               , family = binomial, data = dat,
#'               weights = n, method = "fREML", discrete = TRUE)
#' 
#' # Turning discretization off (on by default for large datasets).
#' set.seed(414) # Setting the seed because qq.gam is doing simulations
#' o <- qq.gam(lr.fit, rep = 10, method = "simul1", CI = "normal", show.reps = TRUE, 
#'             discrete = F, a.replin = list(alpha = 0.1))
#' o # This might take some time!
#' 
#' # Using default discretization
#' set.seed(414)
#' o <- qq.gam(lr.fit, rep = 10, method = "simul1", CI = "normal", show.reps = TRUE, 
#'             a.replin = list(alpha = 0.1))
#' o # Much faster plotting!
#' 
#' # Very coarse discretization
#' set.seed(414)
#' o <- qq.gam(lr.fit, rep = 10, method = "simul1", CI = "normal", show.reps = TRUE,
#'             ngr = 1e2, a.replin = list(alpha = 0.1), a.qqpoi = list(shape = 19))
#' o 
#' 
#' # We can also zoom in at no extra costs (most work already done by qq.gam)
#' zoom(o, xlim = c(-0.25, 0.25), show.reps = TRUE, discrete = TRUE, a.replin = list(alpha = 0.2))
#' }
#' 
qq.gam <- function(o, rep = 10,
                   level = 0.8, 
                   method = c("auto", "simul1", "simul2", "tnormal", "tunif", "normal"),
                   type = c("auto", "deviance", "pearson", "response", "tunif", "tnormal"),
                   CI = c("normal", "quantile", "none"),
                   worm = FALSE,
                   show.reps = FALSE,
                   sortFun = NULL,
                   discrete = NULL,
                   ngr = 1e3,
                   xlim = NULL,
                   ylim = NULL, 
                   a.qqpoi = list(),
                   a.ablin = list(),
                   a.cipoly = list(),
                   a.replin = list(),
                   ...) {
  
  a.all <- .argMaster("qq.gam")
  for(nam in names(a.all)){
    a.all[[nam]] <- .argSetup(a.all[[nam]], get(nam), nam, verbose = FALSE)
  }
  
  CI     <- match.arg(CI, c("normal", "quantile", "none"))
  method <- match.arg(method, c("auto", "simul1", "simul2", "tnormal", "tunif", "normal"))
  type   <- match.arg(type, c("auto", "deviance", "pearson", "response", "tunif", "tnormal"))
  tmp <- .getResTypeAndMethod(o$family$family)
  if (method == "auto") { method = tmp$method }
  if (type == "auto") { type = tmp$type }
  if (level < 0 || level > 1){
    stop("`level' should be between 0 and 1") 
  }
  if (method == "simul2") CI <- "none"
  if (is.null(sortFun))  sortFun  <- function(.x) sort(.x, method = "quick")
  if (is.null(discrete)) discrete <- length(o$y) > 1e4
  if (inherits(o, c("glm", "gam"))) {
    if (is.null(o$sig2)) 
      o$sig2 <- summary(o)$dispersion
  } else {
    stop("'o' is not a glm or gam.")
  }
  o$na.action <- NULL
  P0 <- .compute.qq.gam(o = o, type = type, method = method, CI = CI, 
                       level = level, rep = rep, sortFun = sortFun)
  P1 <- .discretize.qq.gam(P = P0, discrete = discrete, ngr = ngr,
                           CI = (CI != "none"), show.reps = show.reps)
  pl <- .plot.qq.gam(P = P1, CI = (CI != "none"), worm = worm, show.reps = show.reps,
                     xlimit = xlim, ylimit = ylim, a.all = a.all)
  out <- structure(list("ggObj" = pl, "store" = P0),
                   "class" = "qqGam", 
                   "call" = match.call())
  return(out)
}


