#'
#' Checking GAM residuals along one covariate
#' 
#' @description XXX
#' @name gridCheck1D
#' @param gridFun function used to summarize the residuals in each bin. 
#'                By default it is \code{mean(r)*sqrt(length(r)}, where \code{r} is
#'                the vector of residuals in that bin.
#' @param n number of grid intervals.
#' @param level the level of the confidence intervals plotted.
#' @param stand if `none` the residuals in each bin are transformed by \code{gridFun} and
#'              plotted as they are. If `sc` the statistics in each bin are scaled and 
#'              centered using the simulated stats in that bin. If `s` we do only scaling, 
#'              if `c` only centering.
#' @param show.reps if \code{TRUE} the individuals simulated statistics are also plotted.
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_point}.
#' @return An object of class \code{gamLayer}
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
#' check1D(b, "x") + gridCheck1D()
#' 
#' # Heteroscedasticity clearly visible here
#' b <- getSim(b, n = 50)
#' check1D(b, "x") + gridCheck1D(gridFun = sd, stand = "sc") # <- we are scaling and centering
#' # Last point on the right of the rug seems to indicate that a bin is missing.
#' # It is not an error, only on observation falls in that bin, hence the
#' # standard deviation is not defined there.
#' 
#' @importFrom matrixStats colSds
#' @importFrom plyr aaply
#' @rdname gridCheck1D
#' @export gridCheck1D
gridCheck1D <- function(gridFun = NULL, n = 20, level = 0.8, stand = "none", show.reps = TRUE, ...){
  arg <- list(...)
  arg$xtra <- list("gridFun"=gridFun, "n"=n, 
                   "level"=level, "stand"=stand, "show.reps"=show.reps)
  o <- structure(list("fun" = "gridCheck1D",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method 
#' @noRd
gridCheck1D.plotSmoothCheck1D <- function(a){
  
  ### 1. Preparation
  xtra <- a$xtra
  a$xtra <- NULL
  
  if( is.null(xtra$gridFun) ){
    xtra$gridFun <- function(.x){
      .o <- mean(.x) * sqrt(length(.x))
      return( .o )
    }
  }
  
  x <- a$data$res$x
  y <- a$data$res$y
  n <- xtra$n
  level <- xtra$level
  
  ### 2. Computation on grid
  # Bin observed data
  grid <- seq(min(x), max(x), length.out = n)
  inX <- findInterval(x, grid, rightmost.closed = T)
  grX <- tapply(x, inX, mean)     # Averaging x inside each bin
  grY <- tapply(y, inX, xtra$gridFun)
  
  rep <- 0
  sim <- a$data$sim
  if( is.null(sim) ){ # NO simulations
    if( level > 0 ){
      message("level>0 but object does not contain any simulations. See ?getSim.")
    } else {
      if( xtra$stand != "none" ){
        message("stand!=`none` but object does not contain any simulations. See ?getSim.")
      }
    }
  } else {  # YES simulations!
    rep <- nrow( sim )
    
    # Bin simulated data 
    inS <- findInterval(x, grid, rightmost.closed = T)
    lev <- sort( unique(inS) )                      
    # lev <- lev[ (lev != 0) & (lev != n) ]   # Discard x's that fall outside grid      
    
    # Calculate function for each bin and each repetition
    grS <- matrix(NA, rep, length(lev))
    for( ir in 1:rep ){ 
      grS[ir, ] <- sapply(lev, function(.ii) { xtra$gridFun(sim[ir, inS==.ii]) } ) 
    }
    
    # Standardize grS and grY
    if( xtra$stand != "none" ) {
      if(xtra$stand %in% c("c", "sc")){ # Center 
        tmpC <- colMeans(grS)
        tmpC[ is.na(tmpC) ] <- 0
      } else{
        tmpC <- numeric( ncol(grS) )
      }
      if(xtra$stand %in% c("s", "sc")){ # Scale
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
  a$data <- data.frame("x" = grX, "y" = grY)
  a$mapping <- aes(x = x, y = y)
  a$inherit.aes <- FALSE
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE }
  if( is.null(a$shape) ){ a$shape <- 19 }
  
  # Build layers
  out <- list()
  out[[1]] <- do.call("geom_point", a)
  
  if( rep > 0){
    datS <- data.frame("x" = rep(grX, rep), "y" = as.vector(t(grS)))
    if(xtra$show.reps) {
      out[[2]] <- geom_point(data = datS, aes(x = x, y = y), na.rm = TRUE, shape = 46)
    }
    if(level > 0){
      
      datCI <- data.frame("x" = goX, "ll" = conf[1, ], "ul" = conf[2, ])
      out[[3]] <- geom_line(data = datCI, aes(x = x, y = ll), na.rm = TRUE, linetype = 2, colour = "red")
      out[[4]] <- geom_line(data = datCI, aes(x = x, y = ul), na.rm = TRUE, linetype = 2, colour = "red")
    }
  }

  class(out) <- "listOfLayers"

  return( out )
  
}

