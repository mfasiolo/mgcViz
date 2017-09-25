#'
#' Simulating responses from a gamViz object
#' @description ...
#' @param o an object of class \code{gamViz}
#' @param n the number of simulated datasets. A positive integer.
#' @param ... extra arguments to be passed to \code{simualte.gam}
#' @examples 
#' library(mgcViz)
#' 
#' set.seed(2) ## simulate some data... 
#' dat <- gamSim(1,n=400,dist="normal",scale=2)
#' b <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
#' b$store$sim # There is nothing here
#' 
#' b <- getSim(b, n = 50)
#' str(b$store$sim) # Simulated responses now stored here
#' 
#' @importFrom plyr raply aaply
#' @export getSim
#' 
getSim <- function(o, n = 10, ...)
{
  o$store$sim <- simulate(o, n = n, ...)
  
  return( o )
}