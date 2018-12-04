#'
#' Binning and checking GAM residuals
#' 
#' @description This layer bins the residuals, r, according to the value of the corresponding
#'              covariates, x1 and x2. Then the residuals in each bin are summarized using a 
#'              scalar-valued statistic. Confidence intervals for the statistic corresponding
#'              to each bin can be obtained by simulating residuals from the fitted GAM
#'              model, which are then binned and summarized. Mainly useful in conjuction with [check2D]. 
#' @name l_gridCheck2D
#' @param gridFun scalar-valued function used to summarize the residuals in each bin.
#' @param bw numeric vector giving bin width in the vertical and horizontal directions. See the `binwidth`
#'           arguments in \code{?ggplot2::stat_summary_hex}. If left to \code{NA}, it will be set to 1/20
#'           of the ranges of x1 and x2. 
#' @param stand if left to \code{TRUE} then the observed statistic in the i-th cell is normalized using 
#'              the simulated statistics in that same cell. That is, we will actually plot
#'              \code{std_stat = (obs_stat-mean(sim_stat))/sd(sim_stat)}.
#' @param binFun the \code{ggplot2} function used to perform the binning. By default it 
#'               is either [ggplot2::stat_summary_2d] or [ggplot2::stat_summary_hex], depending 
#'               on the class of the covariates x1 and x2.
#' @param ... graphical arguments to be passed to \code{ggplot2::stat_summary_hex}.
#' @return An object of class \code{gamLayer}
#' @examples 
#' library(mgcViz);
#' set.seed(4124)
#' n <- 1e4
#' x <- rnorm(n); y <- rnorm(n);
#' 
#' # Residuals are heteroscedastic w.r.t. x
#' ob <- (x)^2 + (y)^2 + (1*abs(x) + 1)  * rnorm(n)
#' b <- bam(ob ~ s(x,k=30) + s(y, k=30), discrete = TRUE)
#' b <- getViz(b, nsim = 50)
#' 
#' # Don't see much by looking at mean
#' check2D(b, "x", "y") + l_gridCheck2D(gridFun = mean, bw = c(0.4, 0.4))
#' 
#' # Variance pattern along x-axis clearer now
#' check2D(b, "x", "y") + l_gridCheck2D(gridFun = sd, bw = c(0.4, 0.4))
#'  
#' @importFrom matrixStats colSds
#' @importFrom plyr aaply
#' @rdname l_gridCheck2D
#' @export l_gridCheck2D
#' 
#' 
l_gridCheck2D <- function(gridFun = mean, bw = c(NA, NA), stand = TRUE, binFun = NULL, ...){
  arg <- list(...)
  if( length(bw) == 1 ) { bw <- c(bw, bw) }
  arg$xtra <- list("gridFun" = gridFun, "bw" = bw, "stand" = stand, "binFun" = binFun)
  o <- structure(list("fun" = "l_gridCheck2D",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method factor/numeric data
#' @noRd
l_gridCheck2D.Check2DFactorNumeric <- function(a){
  
  if( is.na(a$xtra$bw[1]) ) { a$xtra$bw[1] <- 1 }
  
  if( is.null(a$xtra$binFun) ){ a$xtra$binFun <- "stat_summary_2d" }
  
  .l_gridCheck2D( a )
  
}

######## Internal method factor/factor data
#' @noRd
l_gridCheck2D.Check2DFactorFactor <- function(a){
  
  if( is.na(a$xtra$bw[1]) ) { a$xtra$bw[1] <- 1 }
  if( is.na(a$xtra$bw[2]) ) { a$xtra$bw[2] <- 1 }
  
  if( is.null(a$xtra$binFun) ){ a$xtra$binFun <- "stat_summary_2d" }
  
  .l_gridCheck2D( a )
  
}

######## Internal method for numeric/numeric data
#' @noRd
l_gridCheck2D.Check2DNumericNumeric <- function(a){
  
  if( is.null(a$xtra$binFun) ){ a$xtra$binFun <- "stat_summary_hex" }
  
  .l_gridCheck2D( a )
}

######## General internal method 
#' @noRd
.l_gridCheck2D <- function(a){
  
  ### 1. Preparation
  xtra <- a$xtra
  a$xtra <- NULL
  
  gridFun <- xtra$gridFun
  binFun <- xtra$binFun
  bw <- xtra$bw
  stand <- xtra$stand
  
  # Quantile GAM case: need to get specific gridFun()
  if( !is.null(attr(xtra$gridFun, "Qcheck")) ){
    if( a$data$misc$resType == "y" ){
      message("Using l_gridQCheck2D might not make sense with residual type == \"y\". See ?check2D")
    }
    if( is.null(a$xtra$qu) ){ 
      if( is.null(a$data$misc$qu) ){ stop("Please specify argument qu in the call to l_gridQCheck1D") }
      xtra$qu <- a$data$misc$qu 
    }
    gridFun <- gridFun(.qu = xtra$qu, .stand = stand) # Fixing .qu and .stand values inside closure
    stand <- FALSE # Standardization already happening in gridFun
  }
  
  # Wrapper that allows to compute the gridFun() over the observed and
  # simulated residuals. The observed statistic in each cell is then normalized using
  # the standard deviation of the simulated statistics in the corresponding cell
  getGridFun <- function(.ifun){
    # Internal wrapper  
    .f <- function(.ii, .r, .sim, .stand){

      if( length(.ii) ){
        .r <- .r[ .ii ]
        .o <- .ifun( .r )
        
        if( .stand ){
          if( !is.null(.sim) ){ # Standardize using simulated stats
            sk <- apply(.sim[ , .ii, drop = F], 1, .ifun)
            .o <- (.o - mean(sk)) / sd(sk)
          }
        }
        
        return( .o )
      } else {
        return( NA)
      }
    }
    return( .f )  
  }
  
  gridWrapper <- getGridFun( gridFun )
  
  x <- a$data$res$x
  y <- a$data$res$y
  z <- a$data$res$z   # Observed residuals
  sim <- a$data$sim   # Simulated residuals
  
  if( stand && is.null(sim) ){
    message("stand==TRUE but object does not contain any simulations. See ?getViz.")
  }
  
  # Determine bin widths for the two plots
  if( is.na(bw[1]) ){ bw[1] <- diff(range(x))/20 }
  if( is.na(bw[2]) ){ bw[2] <- diff(range(y))/20 }
  
  ### 2. Plotting
  a$data <- data.frame("x" = x, "y" = y, "z" = 1:length(x))
  a$mapping <- aes(x=x, y=y, z=z)
  a$binwidth <- bw
  a$fun <- gridWrapper
  a$fun.args <- list(".r" = z, ".sim" = sim, ".stand" = stand)
  a$inherit.aes <- FALSE
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE }

  # Build layers
  out <- list()
  out[[1]] <- do.call(binFun, a)
  out[[2]] <- scale_fill_gradientn(colours = viridis(50, begin=0.2), na.value="white")

  class(out) <- "listOfLayers"
  
  return( out )
  
}

