#'
#' Checking GAM simulated residuals or responses
#' 
#' @description This function extracts the residuals or responses of a fitted GAM model, then it compares 
#'              their distribution with that of model-based simulations. 
#' @name check0D
#' @param o an object of class \code{gamViz}.
#' @param type the type of residuals to be used. See [residuals.gamViz]. 
#'             If \code{"type == y"} then the raw observations will be used. 
#' @param maxpo maximum number of residuals points that will be used by layers such as
#'              \code{l_rug()}. If number of datapoints > \code{maxpo}, then a subsample of \code{maxpo} points 
#'              will be taken.
#' @param na.rm if \code{TRUE} missing cases in \code{x} or \code{y} will be dropped out.
#' @param trans function used to transform the observed and simulated residuals or responses. 
#'              It must take a vector of as input, and it must either a vector of the same length or a scalar. 
#' @param useSim if \code{FALSE} then the simulated responses contained in object \code{o} will not be used
#'               by this function or by any of the layers that can be used with its output.
#' @return An object of class \code{c("plotSmooth", "gg")}.
#' @examples 
#' # The variance of the response distribution changes along x2 
#' library(mgcViz)
#' n  <- 400
#' x1 <- runif(n, -1, 1)
#' x2 <- runif(n, -1, 1)
#' dat <- data.frame("x1" = x1, "x2" = x2,
#'                   "y" = sin(3*x1) + 0.5 * x2^2 + pmax(x2, 0.2) * rnorm(n))
#' 
#' # Fit model with constant variance and perform posterior simulations (post = TRUE) 
#' # which take into account smoothing parameter uncertainty (unconditional = TRUE)
#' b <- gamV(y ~ s(x1)+s(x2), data = dat, 
#'           aViz = list(nsim = 50, post = TRUE, unconditional = TRUE))
#' 
#' # Histogram of simulated vs observed residuals: the latter are fat tailed
#' check0D(b) + l_hist() + l_rug()
#' 
#' # Histogram of simulated 4th central moment (~ kurtosis) of simulated residuals.
#' # The vertical line is the 4th moment of the observed residuals
#' check0D(b, trans = function(.y) mean((.y - mean(.y))^4)) + l_dens1D() + l_vline() + l_rug()
#' # Residuals look very fat tails, but the real problem here is the heteroscedasticity 
#' # which can be diagnosted using check1D(b, "x2") + l_gridCheck1D(sd)
#' @rdname check0D
#' @export check0D
#' 
check0D <- function(o, type = "auto", maxpo = 1e4, na.rm = TRUE, trans = NULL, useSim = TRUE){
  
  if( !inherits(o, "gamViz") ){ stop("Argument 'o' should be of class 'gamViz'. See ?getViz") }
  if( !useSim ) { o$store$sim <- NULL }
  
  ### 1. Preparation
  type <- match.arg(type, c("auto", "deviance", "pearson", "scaled.pearson", 
                            "working", "response", "tunif", "tnormal", "y"))
  
  # Get data, responses and type of residuals
  tmp <- .getDataTypeY(o = o, type = type)
  y <- tmp$y
  data <- tmp$data
  type <- tmp$type
  
  # Discard NAs
  if( na.rm ){
    good <- complete.cases(y)
    y <- y[ good ]
  }
  
  ### 2. a) Transform simulated responses to residuals (unless type == "y")
  ###    b) Apply optional transformation to observed and simulated y's
  ###    c) Obtain subsample indexes
  tmp <- .getresidualsTransformSubsample(o = o, y = y, maxpo = maxpo, trans = trans, type = type)
  y <- tmp$y
  sim <- tmp$sim
  sub <- tmp$sub

  ### 3. Build output object
  res <- data.frame("x" = y, "sub" = sub)
  pl <- ggplot(data = res, mapping = aes(x = x)) + theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    labs(x = ifelse(type == "y", "y", "r"))
  
  cls <- .mapVarClass(class(res$x))
  if( cls == "factor" ){ pl <- pl + scale_x_discrete()}
  
  misc <- list("resType" = type, "trans" = trans)
  
  out <- structure(list("ggObj" = pl, 
                        "data" = list("res" = res, 
                                      "sim" = sim, 
                                      "misc" = misc), 
                        "type" = c("Check", "0D", ifelse(length(y)==1, "Scalar", "Vector"), .simpleCap(cls))), 
                   class = c("plotSmooth", "gg"))
  
  return( out )
  
}
