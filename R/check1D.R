#'
#' Checking GAM residuals along one covariate
#' 
#' @description This function extracts the residuals of a fitted GAM model, and orders
#'              them according to the value of a single covariate. Then several visual residuals diagnostics 
#'              can be plotted by adding layers. 
#' @name check1D
#' @param o an object of class \code{gamViz}.
#' @param x should be either a single character or a numeric vector. 
#'          In the first case it should be the name of one of the variables in the dataframe used to fit \code{o}.
#'          In the second case the length of \code{x} should be equal to the length of \code{residuals(o)}.
#' @param type the type of residuals to be used. See [residuals.gamViz].
#' @param maxpo maximum number of residuals points that will be used by layers such as
#'              \code{resRug()} and \code{resPoints()}. If number of datapoints > \code{maxpo},
#'              then a subsample of \code{maxpo} points will be taken.
#' @param na.rm if \code{TRUE} missing cases in \code{x} or \code{y} will be dropped out.
#' @return An object of class \code{c("plotSmooth", "gg")}.
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
#' ck <- check1D(b, "x")
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
#' check1D(b, "x") + l_gridCheck1D(gridFun = sd, show.reps = TRUE)
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
#' @importFrom plyr aaply
#' @rdname check1D
#' @export check1D
#' 
check1D <- function(o, x, type = "auto", maxpo = 1e4, na.rm = TRUE){
  
  if( !inherits(o, "gamViz") ){ stop("Argument 'o' should be of class 'gamViz'. See ?getViz") }
  
  ### 1. Preparation
  type <- match.arg(type, c("auto", "deviance", "pearson", "scaled.pearson", 
                            "working", "response", "tunif", "tnormal"))
  
  # Returns the appropriate residual type for each GAM family
  if( type=="auto" ) { type <- .getResTypeAndMethod(o$family$family)$type }
  
  # Get residuals
  y <- residuals(o, type = type)
  
  xnm <- "x" # If `x` is char, get related vector from dataframe
  if( is.character(x) ){ 
    xnm <- x
    data <- o$model
    if( !(x %in% names(data)) ) stop("(x %in% names(data)) == FALSE")
    x <- xfull <- data[[x]]
  }
  
  if(length(x) != length(y)){ stop("length(x) != length(y)") }
  
  # Discard NAs
  m <- length(x)
  if( na.rm ){
    good <- complete.cases(y, x)
    y <- y[ good ]
    x <- x[ good ]
    m <- length(good)
  }
  
  # Sample if too many points (> maxpo)  
  sub <- if(m > maxpo) { 
    sample( c(rep(T, maxpo), rep(F, m-maxpo)) )
  } else { 
    rep(T, m) 
  }
  
  ### 2. Transform responses to residuals
  sim <- NULL
  if( !is.null(o$store$sim) ){
    sim <- aaply(o$store$sim, 1, 
                 function(.yy){  
                   o$y <- .yy
                   return( residuals(o, type = type) )
                 }) 
  }
    
  ### 3. Build output object
  res <- data.frame("x" = x, "y" = y, "sub" = sub)
  pl <- ggplot(data = res, mapping = aes(x = x, y = y)) + theme_bw() + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
        labs(x = xnm, y = "r")
  
  cls <- .mapVarClass(class(res$x))
  if( cls == "factor" ){ pl <- pl + scale_x_discrete()}
  
  misc <- list("resType" = type)
  
  out <- structure(list("ggObj" = pl, 
                        "data" = list("res" = res, 
                                      "sim" = sim, 
                                      "misc" = misc), 
                        "type" = c("Check", "1D", .simpleCap(cls))), 
                   class = c("plotSmooth", "gg"))

  return( out )
  
}
