#' Checking higher dimensional smooth effects
#' 
#' @description XXX
#' @name check.mgcv.smooth.MD
#' @examples 
#' library(mgcViz)
#' n <- 1e3
#' x <- rnorm(n); y <- rnorm(n); z <- rnorm(n)
#' 
#' ob <- (x-z)^2 + (y-z)^2 + rnorm(n)
#' b <- gam(ob ~ s(x, y, z))
#' v <- getViz(b)
#' 
#' check(v(1), fix = c("z"=1), too.far = 1)
#' @rdname check.mgcv.smooth.MD
#' @export check.mgcv.smooth.MD
check.mgcv.smooth.MD <- function(o, fix, type="auto", binw1=NULL, binw2=NULL, 
                                 gridFun=NULL, nco=40, too.far = NA, xlimit=NULL, ylimit=NULL, 
                                 palette1=viridis(50, begin=0.2), 
                                 palette2=rev(gray.colors(20)), 
                                 acFun=list(NULL, NULL))
{
  if( !("mgcv.smooth.MD" %in% class(o)) ) { stop("\"o\" should be of class \"mgcv.smooth.MD\"") }
  
  if( length(too.far) > 1 ){
    warning("length(too.far) > 1, but `too.far` should be a non-negative scalar")
    too.far <- too.far[1]
  }
  
  .pl <- .check.mgcv.smooth.2D(o=o, type=type, binw1=binw1, binw2=binw2, 
                               gridFun=gridFun, nco=nco, xlimit=xlimit, ylimit=ylimit, 
                               palette1=palette1, palette2=palette2, acFun=acFun, fix=fix,
                               too.far=c(0, too.far))
  
  class(.pl) <- "check.smooth.MD"
  
  return( .pl )
}

