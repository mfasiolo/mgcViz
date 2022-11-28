#'
#' Checking GAM residuals or responses along one covariate
#' 
#' @description This function extracts the residuals of a fitted GAM model, and orders
#'              them according to the value of a single covariate. Then several visual residuals diagnostics 
#'              can be plotted by adding layers. 
#' @name check1D
#' @param o an object of class \code{gamViz}.
#' @param x it can be either a) a single character, b) a numeric vector or c) a list of characters. 
#'           In case a) it should be the name of one of the variables in the dataframe used to fit \code{o}.
#'           In case b) its length should be equal to the length of \code{o$y}.
#'           In case c) it should be a list of names variables in the dataframe used to fit \code{o}.
#' @param type the type of residuals to be used. See [residuals.gamViz]. 
#'             If \code{"type == y"} then the raw observations will be used. 
#' @param maxpo maximum number of residuals points that will be used by layers such as
#'              \code{l_rug()}. If number of datapoints > \code{maxpo},
#'              then a subsample of \code{maxpo} points will be taken.
#' @param na.rm if \code{TRUE} missing cases in \code{x} or \code{y} will be dropped out.
#' @param trans function used to transform the observed and simulated residuals or responses. It must take a vector of 
#'              as input, and must return a vector of the same length. 
#' @param useSim if \code{FALSE} then the simulated responses contained in object \code{o} will not be used
#'               by this function or by any of the layers that can be used with its output.
#' @return The function will return an object of class \code{c("plotSmooth", "gg")}, unless argument \code{x} is a
#'         list. In that case the function will return an object of class \code{c("plotGam", "gg")} containing 
#'         a checking plot for each variable. 
#' @importFrom stats complete.cases
#' @examples 
#' ### Example 1: diagnosing heteroscedasticity
#' library(mgcViz);
#' set.seed(4124)
#' n <- 1e4
#' x <- rnorm(n); y <- rnorm(n);
#' 
#' # Residuals are heteroscedastic w.r.t. x
#' ob <- (x)^2 + (y)^2 + (0.2*abs(x) + 1)  * rnorm(n)
#' b <- bam(ob ~ s(x,k=30) + s(y, k=30), discrete = TRUE)
#' b <- getViz(b)
#' 
#' # Look at residuals along "x"
#' ck <- check1D(b, "x", type = "tnormal")
#' 
#' # Can't see that much
#' ck + l_dens(type = "cond", alpha = 0.8) + l_points() + l_rug(alpha = 0.2)
#' 
#' # Some evidence of heteroscedasticity
#' ck + l_densCheck()
#' 
#' # Compare observed residuals std dev with that of simulated data,
#' # heteroscedasticity is clearly visible
#' b <- getViz(b, nsim = 50)
#' check1D(b, "x") + l_gridCheck1D(gridFun = sd, showReps = TRUE)
#' 
#' # This also works with factor or logical data
#' fac <- sample(letters, n, replace = TRUE)
#' logi <- sample(c(TRUE, FALSE), n, replace = TRUE)
#' b <- bam(ob ~ s(x,k=30) + s(y, k=30) + fac + logi, discrete = TRUE)
#' b <- getViz(b, nsim = 50)
#' 
#' # Look along "fac"
#' ck <- check1D(b, "fac") 
#' ck + l_points() + l_rug() 
#' ck + l_gridCheck1D(gridFun = sd)
#' 
#' # Look along "logi"
#' ck <- check1D(b, "logi") 
#' ck + l_points() + l_rug() 
#' ck + l_gridCheck1D(gridFun = sd)
#' 
#' @importFrom matrixStats colSds
#' @importFrom plyr aaply llply
#' @rdname check1D
#' @export check1D
#' 
check1D <- function(o, x, type = "auto", maxpo = 1e4, na.rm = TRUE, trans = NULL, useSim = TRUE){
  
  if( !inherits(o, "gamViz") ){ stop("Argument 'o' should be of class 'gamViz'. See ?getViz") }
  if( !useSim ) { o$store$sim <- NULL }
  
  if( is.list(x) ){
    out <- lapply(x, function(.x) check1D(o = o, x = .x, type = type, maxpo = maxpo, 
                                          na.rm = na.rm, trans = trans, useSim = useSim))
    out  <- structure(list("plots" = out, "empty" = TRUE), 
                           "class" = c("plotGam", "gg"))
    return( out )
  }
  
  ### 1. Preparation
  type <- match.arg(type, c("auto", "deviance", "pearson", "scaled.pearson", 
                            "working", "response", "tunif", "tnormal", "y"))
  
  # Get data, responses and type of residuals
  tmp <- .getDataTypeY(o = o, type = type)
  y <- as.matrix( tmp$y )
  data <- tmp$data
  type <- tmp$type
  dy <- ncol(y)
  
  # Get the covariate of interest from the dataset
  xnm <- "x" # If `x` is char, get related vector from dataframe
  if( is.character(x) ){ 
    xnm <- x
    if( !(x %in% names(data)) ) stop("(x %in% names(data)) == FALSE")
    x <- xfull <- data[[x]]
  }
  x <- as.vector( x ) # Needed for functional GAMS
  
  if(length(x) != nrow(y)){ stop("length(x) != nrow(y)") }
  
  # Discard NAs
  if( na.rm ){
    good <- complete.cases(y, x)
    y <- y[good, ]
    x <- x[ good ]
  }
  
  ### 2. a) Transform simulated responses to residuals (unless type == "y")
  ###    b) Apply optional transformation to observed and simulated y's
  ###    c) Obtain subsample indexes
  tmp <- .getresidualsTransformSubsample(o = o, y = y, maxpo = maxpo, trans = trans, type = type)
  y <- tmp$y
  sim <- tmp$sim
  sub <- tmp$sub
  
  ### 3. Build output object
  res <- data.frame("x" = x, "sub" = sub, stringsAsFactors = TRUE)
  res$y <- y # Add y now because it might be a matrix
  pl <- ggplot(data = res, mapping = aes(x = x, y = y)) + theme_bw() + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
        labs(x = xnm, y = ifelse(type == "y", "y", "r"))
  
  cls <- .mapVarClass(class(res$x))
  if( cls == "factor" ){ pl <- pl + scale_x_discrete()}
  
  misc <- list("resType" = type, "trans" = trans, "modelClass" = class(o))
  if( inherits(o, "qgam") ) { misc$qu = o$family$getQu() }
  
  out <- structure(list("ggObj" = pl, 
                        "data" = list("res" = res, 
                                      "sim" = sim, 
                                      "misc" = misc), 
                        "type" = c("Check", "1D", .simpleCap(cls))), 
                   class = c("plotSmooth", "gg"))

  return( out )
  
}
