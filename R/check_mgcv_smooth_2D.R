#' Checking two dimensional smooth effects
#' 
#' @description XXX
#' @name check.mgcv.smooth.2D
#' @examples 
#' library(mgcViz);
#' 
#' # Simulate data from Rosenbrock function
#' n <- 1e3
#' X <- data.frame("x1"=rnorm(n, 0.5, 0.5), "x2"=rnorm(n, 1.5, 1))
#' X$y <- (1-X$x1)^2 + 100*(X$x2 - X$x1^2)^2 + rnorm(n, 0, 2)
#' b <- gam(y ~ te(x1, x2, k = 5), data = X, method = "REML")#, discrete = T)
#' 
#' v <- getViz(b)
#' o <- v(1) 
#' 
#' # Check residuals: k is too low to model the effect of x1 and x2 correctly,  
#' # hence the residuals are far from iid.
#' a <- check(o, xlim=c(-1, 1), ylim=c(0, 3))
#' 
#' a # calls print.check.smooth.2D
#' @rdname check.mgcv.smooth.2D
#' @importFrom dplyr filter sample_n
#' @export check.mgcv.smooth.2D
check.mgcv.smooth.2D <- function(o, type="auto", rep = 0, binw1=NULL, binw2=NULL, 
                                 gridFun=NULL, addCont = TRUE, nco=40,
                                 xlim=NULL, ylim=NULL, 
                                 palette1=viridis(50, begin=0.2), 
                                 palette2=rev(gray.colors(20)), 
                                 acFun=list(NULL, NULL))
{
  if( !("mgcv.smooth.2D" %in% class(o)) ) { stop("\"o\" should be of class \"mgcv.smooth.2D\"") }
  
  .pl <- .check.mgcv.smooth.2D(o=o, type=type, rep=rep, binw1=binw1, binw2=binw2, 
                               gridFun=gridFun, addCont=addCont, nco=nco, xlimit=xlim, ylimit=ylim, 
                               palette1=palette1, palette2=palette2, acFun=acFun, too.far=0)
  
  class(.pl) <- "check.smooth.2D"
  
  return( .pl )
}



