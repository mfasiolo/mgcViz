#'
#' Checking GAM residuals along one covariate
#' 
#' @description XXX
#' @name l_gridCheck2D
#' @param gridFun function used to summarize the residuals in each bin. By default it is `mean`.
#' @param bw numeric vector giving bin width in both vertical and horizontal directions. See the `binwidth`
#'           arguments in \code{?ggplot2::stat_summary_hex}. If left to \code{NA} is will set to one 20th
#'           of the ranges of x and y. 
#' @param stand if left to \code{TRUE} then the observed statistic in the i-th cell is normalized using 
#'              the simulated statistics in that same cell. That is we are actually plotting
#'              \code{std_stat = (obs_stat-mean(sim_stat))/sd(sim_stat)}.
#' @param ... graphical arguments to be passed to \code{ggplot2::stat_summary_hex}.
#' @return An object of class \code{gamLayer}
#' @examples 
#' 
#' library(mgcViz);
#' set.seed(4124)
#' n <- 1e4
#' x <- rnorm(n); y <- rnorm(n);
#' 
#' # Residuals are heteroscedastic w.r.t. x
#' ob <- (x)^2 + (y)^2 + (1*abs(x) + 1)  * rnorm(n)
#' b <- bam(ob ~ s(x,k=30) + s(y, k=30), discrete = TRUE)
#' b <- getSim(b, n = 50)
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
l_gridCheck2D <- function(gridFun = mean, bw = c(NA, NA), stand = TRUE, ...){
  arg <- list(...)
  arg$xtra <- list("gridFun"=gridFun, "bw"=bw, "stand"=stand)
  o <- structure(list("fun" = "l_gridCheck2D",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method 
#' @noRd
l_gridCheck2D.plotSmoothCheck2D <- function(a){
  
  ### 1. Preparation
  xtra <- a$xtra
  a$xtra <- NULL
  
  gridFun <- xtra$gridFun
  bw <- xtra$bw
  stand <- xtra$stand
  
  # Wrapper that allows to compute the gridFun() over the observed and
  # simulated residuals. The observed statistic in each cell is than normalized using
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
    message("stand==TRUE but object does not contain any simulations. See ?getSim.")
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
  out[[1]] <- do.call("stat_summary_hex", a)
  out[[2]] <- scale_fill_gradientn(colours = viridis(50, begin=0.2), na.value="white")

  class(out) <- "listOfLayers"
  
  return( out )
  
}

