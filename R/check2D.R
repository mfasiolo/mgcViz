#'
#' Checking GAM residuals along two covariates
#' 
#' @description This function extracts the residuals of a fitted GAM model, and plots them
#'              according to the values of two covariates. Then several visual residuals diagnostics 
#'              can be plotted by adding layers. 
#' @name check2D
#' @param o an object of class \code{gamViz}.
#' @param x1 should be either a single character or a numeric vector. 
#'          In the first case it should be the name of one of the variables in the dataframe used to fit \code{o}.
#'          In the second case the length of \code{x1} should be equal to the length of \code{residuals(o)}.
#' @param x2 same as \code{x2}, but this will appear on the y-axis.
#' @param type the type of residuals to be used. See [residuals.gamViz]. 
#'             If \code{"type == y"} then the raw observations will be used. 
#' @param maxpo maximum number of residuals points that will be used by layers such as
#'              \code{l_rug()}. If number of datapoints > \code{maxpo},
#'              then a subsample of \code{maxpo} points will be taken.
#' @param na.rm if \code{TRUE} missing cases in \code{x} or \code{y} will be dropped out 
#' @param trans function used to transform the observed and simulated residuals or responses. It must take a vector of 
#'              as input, and must return a vector of the same length. 
#' @return An object of class \code{c("plotSmooth", "gg")}.
#' @examples 
#' library(mgcViz);
#' #### Example 1: Rosenbrock function
#' # Simulate data
#' n <- 1e4
#' X <- data.frame("x1"=rnorm(n, 0.5, 0.5), "x2"=rnorm(n, 1.5, 1))
#' X$y <- (1-X$x1)^2 + 100*(X$x2 - X$x1^2)^2 + rnorm(n, 0, 2)
#' b <- bam(y ~ te(x1, x2, k = 5), data = X, discrete = TRUE)
#' b <- getViz(b, nsim = 50)
#' 
#' # Plot joint density of observed covariate x1 and x2
#' check2D(b, x1 = "x1", x2 = "x2") + l_rug() + l_dens(type="joint", alpha=0.6) + l_points()
#' 
#' # Look at how mean of residuals varies across x1 and x2
#' check2D(b, x1 = "x1", x2 = "x2") + l_gridCheck2D() + l_points()
#' 
#' # Can't see much in previous plot, let's zoom in central area, where most
#' # data is. Here we can clearly see that the mean model is mispecified
#' check2D(b, x1 = "x1", x2 = "x2") + l_gridCheck2D(bw = c(0.05, 0.1)) +
#'                                    xlim(-1, 1) + ylim(0, 3)
#' # Fit can be improved by increasing k in the bam() call
#' 
#' #### Example 2: checking along factor variables
#' # Simulate data where variance changes along factor variable "fac"
#' n <- 1e4
#' X <- data.frame("x1"=rnorm(n, 0.5, 0.5), "x2"=rnorm(n, 1.5, 1))
#' X$fac <- as.factor( sample(letters, n, replace = TRUE) )
#' X$fac2 <- as.factor( sample(c("F1", "F2", "F3", "F4", "F5"), n, replace = TRUE) )
#' X$y <- (1-X$x1)^2 + 5*(X$x2 - X$x1^2)^2 + 0.1*as.numeric(X$fac) * rnorm(n, 0, 2)
#' b <- bam(y ~ te(x1, x2, k = 5) + fac + fac2, data = X, discrete = TRUE)
#' b <- getViz(b, nsim = 50)
#' 
#' # Check standard deviation of residuals along covariates "x1" and "fac"
#' a <- check2D(b, x1 = "x2", x2 = "fac")
#' a + l_gridCheck2D(gridFun = sd) + l_rug() + l_points() 
#' 
#' # Points and rug are jittered by default, but we can over-ride this
#' a + l_rug(position = position_jitter(width = 0, height = 0)) + 
#'   l_points(position = position_jitter(width = 0, height = 0)) 
#' 
#' # Check standard deviation of residuals along the two factor variables
#' a <- check2D(b, x1 = "fac", x2 = "fac2")
#' a + l_gridCheck2D(gridFun = sd, bw = c(1, 4)) + l_rug() + l_points() 
#' 
#' @importFrom utils combn
#' @rdname check2D
#' @export check2D
#' 
check2D <- function(o, x1, x2, type = "auto", maxpo = 1e4, na.rm = TRUE, trans = NULL)
{
  if( !inherits(o, "gamViz") ){ stop("Argument 'o' should be of class 'gamViz'. See ?getViz") }
  
  # If x1 or x2 is not specified, and the one that is specified is a list, we 
  # create all possible combinations of x1 and x2
  anyMis <- which( c(missing(x1), missing(x2)) )
  if( length(anyMis) ){
    if( length(anyMis) == 2 ){ stop("Please specify x1 and x2") }
    if( anyMis == 1 ) { x1 <- x2 }
    if( !is.list(x1) ) { stop("Please specify x1 and x2") } 
    np <- length( x1 )
    cmb <- combn(np, 2)
    x2 <- x1[ cmb[2, , drop = TRUE] ] # This before...
    x1 <- x1[ cmb[1, , drop = TRUE] ] # ...that
  }
  
  if( is.list(x1) && is.list(x2) ){
    np <- length( x1 )
    if( np != length(x2) ) { 
      message("check2D: length(x1) != length(x2), not all pairs of variables will be plotted")
      np <- min(np, length(x2))
    }
    out <- lapply(1:np, function(.ii) check2D(o = o, x1 = x1[[.ii]], x2 = x2[[.ii]], type = type, 
                                              maxpo = maxpo, na.rm = na.rm, trans = trans))
    out  <- structure(list("plots" = out, "empty" = TRUE), 
                      "class" = c("plotGam", "gg"))
    return( out )
  }
  
  ### 1. Preparation
  type <- match.arg(type, c("auto", "deviance", "pearson", "scaled.pearson", 
                            "working", "response", "tunif", "tnormal", "y"))
  
  # Get data, responses and type of residuals
  tmp <- .getDataTypeY(o = o, type = type)
  y <- tmp$y
  data <- tmp$data
  type <- tmp$type

  # Get covariate vectors: x1 or x2 are chars, get related vectors from dataframe
  xnm1 <- "x1"
  if( is.character(x1) ){ 
    xnm1 <- x1
    if( !(x1 %in% names(data)) ) stop("(x1 %in% names(data)) == FALSE")
    x1 <- xfull1 <- o$model[[x1]]
  }
  
  xnm2 <- "x2"
  if( is.character(x2) ){ 
    xnm2 <- x2
    if( !(x2 %in% names(data)) ) stop("(x2 %in% names(data)) == FALSE")
    x2 <- xfull2 <- o$model[[x2]]
  }
  
  if( length(x1) != length(y) || length(x2) != length(y) ){ 
    stop("x1 and x2 should be a vector of same lenght as residuals(o)") 
  }
  
  # Discard NAs
  if( na.rm ){
    good <- complete.cases(y, x1, x2)
    y <- y[ good ]
    x1 <- x1[ good ]
    x2 <- x2[ good ]
  }
  
  ### 2. a) Transform simulated responses to residuals (unless type == "y")
  ###    b) Apply optional transformation to observed and simulated y's
  ###    c) Obtain subsample indexes
  tmp <- .getresidualsTransformSubsample(o = o, y = y, maxpo = maxpo, trans = trans, type = type)
  y <- tmp$y
  sim <- tmp$sim
  sub <- tmp$sub
  
  cls1 <- .mapVarClass( class(x1) )
  cls2 <- .mapVarClass( class(x2) )
  # If x2 is factor and x1 is not we swap them. So factor variable is always on x axis.
  if( cls2 == "factor" && cls1 != "factor" ){
    message("check2D likes to have factor variables on x axis: I am swapping the coordinates.")
    tmp <- x1; x1 <- x2; x2 <- tmp
    tmp <- xnm1; xnm1 <- xnm2; xnm2 <- tmp
    cls2 <- cls1; cls1 <- "factor"
  }
  
  ### 3. Build output object
  res <- data.frame("x" = x1, "y" = x2, "z" = y, "sub" = sub)
  pl <- ggplot(data = res, mapping = aes(x = x, y = y, z = z)) + theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    labs(x = xnm1, y = xnm2)

  if( cls1 == "factor" ){ pl <- pl + scale_x_discrete()}
  if( cls2 == "factor" ){ pl <- pl + scale_y_discrete()}

  misc <- list("resType" = type, "vnam" = c(xnm1, xnm2), "trans" = trans, "modelClass" = class(o))
  if( inherits(o, "qgam") ) { misc$qu = o$family$getQu() }
  
  out <- structure(list("ggObj" = pl, 
                        "data" = list("res" = res, 
                                      "sim" = sim, 
                                      "misc" = misc), 
                        "type" = c("Check", "2D", .simpleCap(cls1), .simpleCap(cls2))), 
                   class = c("plotSmooth", "gg"))
  
  return( out )
}



