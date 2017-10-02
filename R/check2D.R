#'
#' Checking GAM residuals along two covariates
#' 
#' @description XXX
#' @name check2D
#' @param o an object of class \code{gamObject} or \code{gamViz}.
#' @param x1 should be either a single character or a numeric vector. 
#'          In the first case it should be the name of one of the variables in the dataframe used to fit \code{o}.
#' @param x2 same as \code{x2}, but this will appear on the y-axis.
#' @param type the type of residuals to be used. See \code{?residuals.gam}
#' @param maxpo maximum number of residuals points that will be used by layers such as
#'              \code{resRug()} and \code{resPoints()}. If number of datapoints > \code{maxpo},
#'              then a subsample of \code{maxpo} points will be taken.
#' @param na.rm if \code{TRUE} missing cases in \code{x} or \code{y} will be dropped out 
#' @return An object of class \code{c("plotSmooth", "Check", "2D")}.
#' @examples 
#' library(mgcViz);
#' 
#' # Simulate data from Rosenbrock function
#' n <- 1e4
#' X <- data.frame("x1"=rnorm(n, 0.5, 0.5), "x2"=rnorm(n, 1.5, 1))
#' X$y <- (1-X$x1)^2 + 100*(X$x2 - X$x1^2)^2 + rnorm(n, 0, 2)
#' b <- bam(y ~ te(x1, x2, k = 5), data = X, discrete = T)
#' b <- getViz(b)
#' 
#' # Plot joint density of observed covariate x1 and x2
#' check2D(b, x1 = "x1", x2 = "x2") + l_rug() + l_dens(type="joint", alpha=0.6) + l_points() 
#' 
#' # Look at how mean of residuals varies across x1 and x2
#' b <- getSim(b) # Simulate some residuals to standardise (see ?gridCheck2D)
#' check2D(b, x1 = "x1", x2 = "x2") + l_gridCheck2D() + l_points()
#' 
#' # Can't see much in previous plot, let's zoom in central area, where most
#' # data is. Here we can clearly see that the mean model is mispecified
#' check2D(b, x1 = "x1", x2 = "x2") + l_gridCheck2D(bw = c(0.05, 0.1)) + 
#'                                    xlim(-1, 1) + ylim(0, 3)
#' # Fit can be improved by increasing k in the bam() call
#' 
#' @rdname check2D
#' @importFrom dplyr filter sample_n
#' @export check2D
#' 
check2D <- function(o, x1, x2, bw = NULL, type = "auto", maxpo = 1e4, na.rm = TRUE)
{
  ### 1. Preparation
  type <- match.arg(type, c("auto", "deviance", "pearson", "scaled.pearson", 
                            "working", "response", "tunif", "tnormal"))
  
  # Returns the appropriate residual type for each GAM family
  if( type=="auto" ) { type <- .getResTypeAndMethod(o$family$family)$type }
  
  # Get residuals
  y <- residuals(o, type = type)

  # Get covariate vectors: x1 or x2 are chars, get related vectors from dataframe
  xnm1 <- "x1"
  if( is.character(x1) ){ 
    xnm1 <- x1
    if( !(x1 %in% names(o$model)) ) stop("(x1 %in% names(data)) == FALSE")
    x1 <- xfull1 <- o$model[[x1]]
  }
  
  xnm2 <- "x2"
  if( is.character(x2) ){ 
    xnm2 <- x2
    if( !(x2 %in% names(o$model)) ) stop("(x2 %in% names(data)) == FALSE")
    x2 <- xfull2 <- o$model[[x2]]
  }
  
  if( length(x1) != length(y) || length(x2) != length(y) ){ 
    stop("x1 and x2 should be a vector of same lenght as residuals(o)") 
  }
  
  # Discard NAs
  m <- length(x1)
  if( na.rm ){
    good <- complete.cases(y, x1, x2)
    y <- y[ good ]
    x1 <- x1[ good ]
    x2 <- x2[ good ]
    m <- length( good )
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
  res <- data.frame("x" = x1, "y" = x2, "z" = y, "sub" = sub)
  pl <- ggplot(data = res, mapping = aes(x = x, y = y, z = z)) + theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    labs(x = xnm1, y = xnm2)
  
  misc <- list("type" = type, "vnam" = c(xnm1, xnm2))
  
  out <- structure(list("ggObj" = pl, 
                        "data" = list("res" = res, 
                                      "sim" = sim, 
                                      "misc" = misc)), 
                   class = c("plotSmooth", "Check", "2D"))
  
  return( out )
}



