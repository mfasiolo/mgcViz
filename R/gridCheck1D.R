#'
#' Checking GAM residuals along one covariate
#' 
#' @description XXX
#' @name gridCheck1D
#' @examples 
#' library(mgcViz);
#' set.seed(4124)
#' n <- 1e4
#' x <- rnorm(n); y <- rnorm(n);
#' 
#' # Residuals are heteroscedastic w.r.t. x
#' ob <- (x)^2 + (y)^2 + (0.2*abs(x) + 1)  * rnorm(n)
#' b <- bam(ob ~ s(x,k=30) + s(y, k=30), discrete = TRUE)
#' 
#' # Don't see much by looking at mean
#' gridCheck1D(b, "x", rep = 50, gridFun = mean, 
#'             stand = "sc") # <- we are scaling and standardizing
#' 
#' # Clearly visible here
#' gridCheck1D(b, "x", rep = 50, gridFun = sd)
#' # Last point on the right of the rug seems to indicate that a bin is missing.
#' # It is not an error, only on observation falls in that bin, hence the
#' # standard deviation is not defined there.
#' 
#' @importFrom matrixStats colSds
#' @importFrom plyr aaply
#' @rdname gridCheck1D
#' @export gridCheck1D
gridCheck1D <- function(o, x, y=NULL, gridFun = NULL, ngr = 20, rep = 10, type="auto",
                        level = 0.8, stand = "none", show.reps = TRUE, xlim=NULL, 
                        ylim=NULL, shape1 = 19, shape2 = 46)
{
  ### 1. Preparation
  type <- match.arg(type, c("auto", "deviance", "pearson", "scaled.pearson", 
                            "working", "response", "tunif", "tnormal"))
  
  # Returns the appropriate residual type for each GAM family
  if( type=="auto" ) { type <- mgcViz:::.getResTypeAndMethod(o$family$family)$type }
  
  if( is.null(gridFun) ){
    gridFun <- function(.x){
      .o <- mean(.x) * sqrt(length(.x))
      return( .o )
    }
  }
  
  # Get residuals or transformed responses
  if( is.null(y) ){ y <- residuals(o, type = type) }
  
  xnm <- "x"
  if( is.character(x) ){ # Get data from dataframe
    xnm <- x
    data <- o$model
    if( !(x %in% names(data)) ) stop("(x %in% names(data)) == FALSE")
    x <- xfull <- data[[x]]
  }
  
  if(length(x) != length(y)){ stop("length(x) != length(y)") }
  
  # Discard data outside boundaries
  if( is.null(xlim) ){ # xlim
    xlim <- range(x) 
  } else {
    tmp <- findInterval(x, xlim) == 1
    x <- x[ tmp ]  
    y <- y[ tmp ]
  }
  
  if( is.null(ylim) ){ # ylim
    ylim <- range(y) 
  } else { 
    tmp <- findInterval(y, ylim) == 1
    x <- x[ tmp ]  
    y <- y[ tmp ] 
  }
  
  # Discard NAs
  good <- complete.cases(y, x)
  y <- y[ good ]
  x <- x[ good ]
  m <- length(good)
  
  ### 2. Computation on grid
  # Bin observed data
  grid <- seq(min(x), max(x), length.out = ngr)
  inX <- findInterval(x, grid, rightmost.closed = T)
  grX <- tapply(x, inX, mean)     # Averaging x inside each bin
  grY <- tapply(y, inX, gridFun)
  
  if( rep > 0 ){
    # Simulate data residuals from model
    sim <- simulate(o, n = rep)
    sim <- aaply(sim, 1, 
                 function(.yy){  
                   o$y <- .yy
                   return( residuals(o, type = type) )
                 }) 
    
    # Bin simulated data 
    inS <- findInterval(xfull, grid, rightmost.closed = T)
    lev <- sort( unique(inS) )                      
    lev <- lev[ (lev != 0) & (lev != ngr) ]   # Discard x's that fall outside grid      
    
    # Calculate function for each bin and each repetition
    grS <- matrix(NA, rep, length(lev))
    for( ir in 1:rep ){ 
      grS[ir, ] <- sapply(lev, function(.ii) { gridFun(sim[ir, inS==.ii]) } ) 
    }
    
    # Standardize grS and grY
    if( stand != "none" ) {
      if(stand %in% c("c", "sc")){ # Center 
       tmpC <- colMeans(grS)
       tmpC[ is.na(tmpC) ] <- 0
      } else{
        tmpC <- numeric( ncol(grS) )
      }
      if(stand %in% c("s", "sc")){ # Scale
       tmpS <- colSds(grS)
       tmpS[ is.na(tmpS) ] <- 1 
      } else {
        tmpS <- rep(1, ncol(grS))
      }
      grS <- scale(grS, center = tmpC, scale = tmpS)
      grY <- (grY - tmpC) / tmpS
    }
    
    # Compute confidence intervals
    conf <- NULL
    if( level > 0 ){ 
      conf <- apply(grS, 2, quantile, c(0.5 - level/2, 0.5 + level/2), na.rm = T) 
      good <- !is.na( grY ) # Discard NAs
      goX <- grX[ good ]
      if( level>0 ) { conf <- conf[ , good] }
      }
    
  }
  
  ### 3. Plotting
  dat <- data.frame("x" = grX, "y" = grY)
  .pl <- ggplot(data = dat, aes(x=x, y=y)) 
  
  if( rep > 0){
    datS <- data.frame("x" = rep(grX, rep), "y" = as.vector(t(grS)))
    if(show.reps) { 
      .pl <- .pl + geom_point(data = datS, na.rm = TRUE, shape = shape2) 
    }
    if(level > 0){

      datCI <- data.frame("x" = goX, "ll" = conf[1, ], "ul" = conf[2, ])
      .pl <- .pl + geom_line(data = datCI, aes(x = x, y = ll), na.rm = TRUE, linetype = 2, colour = "red")
      .pl <- .pl + geom_line(data = datCI, aes(x = x, y = ul), na.rm = TRUE, linetype = 2, colour = "red")
    }
  }
  
  .pl <- .pl + geom_point(na.rm = TRUE, shape = shape1) + labs(x = xnm, y = "f(r)")

  return( .pl + theme_bw() )
 
}