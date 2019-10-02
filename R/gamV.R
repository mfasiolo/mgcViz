########
#' Fit a GAM model and get a gamViz object
#' 
#' @description These are wrapper that fits a GAM model using [mgcv::gam] or [mgcv::bam] and 
#'              converts it to a \code{gamViz} object using the [getViz] function.
#'              It is essentially a shortcut.
#'              
#' @param formula,family,data,method same arguments as in [mgcv::gam] or [mgcv::bam].
#' @param aGam list of further arguments to be passed to [mgcv::gam] or [mgcv::bam].
#' @param aViz list of arguments to be passed to [getViz].
#' @return An object of class "gamViz" which can, for instance, be plotted using [plot.gamViz].
#' @name gamV
#' @examples 
#' ##### gam example
#' # Simulate data
#' library(mgcViz)
#' set.seed(2) ## simulate some data...
#' dat <- gamSim(1,n=1000,dist="normal",scale=2)
#' 
#' # Fit GAM and get gamViz object
#' b <- gamV(y~s(x0)+s(x1, x2)+s(x3), data = dat, 
#'           aGam = list(scale = 2), aViz = list("nsim" = 20))
#' 
#' # This is equivalent to doing
#' # 1. Fit GAM
#' # b <- gam(y~s(x0)+s(x1, x2)+s(x3), data=dat, method="REML", scale = 2)
#' # 2. Convert to gamViz object
#' # b <- getViz(b, nsim = 20)
#' 
#' # Either way, we plot first and third effects by doing
#' print(plot(b, select = c(1, 3)), pages = 1)
#' 
#' ##### bam example
#' # Simulate data
#' library(mgcViz)
#' set.seed(2) ## simulate some data...
#' dat <- gamSim(1,n=2000,dist="normal",scale=2)
#' 
#' # Fit using bam() and get gamViz object
#' b <- bamV(y~s(x0)+s(x1, x2)+s(x3), data = dat, 
#'           aGam = list(discrete = TRUE), aViz = list("nsim" = 0))
#'           
#' # Either way, we plot first and third effects by doing
#' print(plot(b, select = c(2)), pages = 1)
#' @importFrom stats gaussian
#' @rdname gamV
#' @export gamV
#
gamV <- function(formula, family = gaussian(), data = list(), method = "REML", aGam = list(), aViz = list()){
  
  obj <- do.call("gam", c(list("formula" = formula, "family" = family, "data" = quote(data), "method" = method), aGam))
  
  obj <- do.call("getViz", c(list("o" = obj), aViz))
  
  # Make sure that the stored function call refers to the name of the data set provided 
  # by the user to gamV (and available in environment where gamV was called), not just 
  # to "data" (as in the call to gam via do.call)
  obj$call$data <- match.call()$data
  
  return( obj )
  
}


