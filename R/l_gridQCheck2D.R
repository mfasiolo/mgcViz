#'
#' Binning and checking QGAM residuals
#' 
#' @description This layer bins the residuals, r, according to the value of the corresponding
#'              covariates, x1 and x2. Then we calculate the proportion of negative residuals 
#'              in each bin, which should not deviate too much from the theoretical proportion (eg 0.5 if
#'              we fit the median). Mainly useful in conjuction with [check2D]. 
#' @name l_gridQCheck2D
#' @param qu the quantile of interest. Should be in (0, 1).
#' @param bw numeric vector giving bin width in the vertical and horizontal directions. See the `binwidth`
#'           arguments in \code{?ggplot2::stat_summary_hex}. If left to \code{NA}, it will be set to 1/20
#'           of the ranges of x1 and x2. 
#' @param stand if left to \code{TRUE} then the observed proportion of negative residuals \code{p_hat} in the i-th cell 
#'              is normalized using the standard error \code{se = sqrt(qu(1-qu)/n)}, where \code{n} is the number of 
#'              observation in that cell. That is, if \code{stand=TRUE} we plot \code{(p_hat-qu)/se} rather than 
#'              simply \code{p_hat}.
#' @param binFun the \code{ggplot2} function used to perform the binning. By default it 
#'               is either [ggplot2::stat_summary_2d] or [ggplot2::stat_summary_hex], depending 
#'               on the class of the covariates x1 and x2.
#' @param ... graphical arguments to be passed to \code{ggplot2::stat_summary_hex}.
#' @return An object of class \code{gamLayer}
#' @examples 
#' library(mgcViz);
#' set.seed(4124)
#' n <- 4e2 
#' dat <- data.frame(x = rnorm(n), y = rnorm(n))
#' 
#' # Simulate some data, residuals are heteroscedastic w.r.t. x
#' dat$ob <- (dat$x)^2 + (dat$y)^2 + (0.2*abs(dat$x) + 1)  * rnorm(n)
#' b <- qgamV(ob ~ x + s(y), qu = 0.3, data = dat)
#' 
#' # We have a residual pattern along x (increase n above to 
#' # see the problem more clearly) 
#' check2D(b, "x", "y") + l_gridQCheck2D(qu = 0.3, bw = c(0.4, 0.4))
#' 
#' # We need a smooth wrt x to make the pattern disappear
#' \dontrun{
#' b1 <- qgamV(ob ~ s(x) + s(y), qu = 0.3, data = dat)
#'  
#' check2D(b1, "x", "y") + l_gridQCheck2D(qu = 0.3, bw = c(0.4, 0.4))
#' }
#' 
#' @rdname l_gridQCheck2D
#' @export l_gridQCheck2D
#' 
l_gridQCheck2D <- function(qu = NULL, bw = c(NA, NA), stand = TRUE, binFun = NULL, ...){
  
  .closure <- function(.qu, .stand){
    force(.qu)    # Need to force evaluation now
    force(.stand)
    .tmp <- function(.x){ 
      .p <- mean(.x <= 0)
      if( .stand ){
        return( (.p-.qu)/sqrt(.qu*(1-.qu)/length(.x)) ) 
      }else{
        return( .p )
      }
    }
    return( .tmp )
  }
  
  attr(.closure, "Qcheck") <- TRUE # Used to signal to l_gridCheck2D that we are interested in quantiles
  attr(.closure, "qu") <- qu 
  
  o <- l_gridCheck2D(gridFun = .closure, bw = bw, stand = stand, binFun = binFun, ...)
  
  return(o)

}