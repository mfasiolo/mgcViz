#' Convert gamViz object to gamObject 
#' 
#' @description XXX
#' @name getGam
#' @examples 
#' library(mgcViz)
#' set.seed(2) ## simulate some data...
#' dat <- gamSim(1,n=1000,dist="normal",scale=2)
#' b <- gam(y~s(x0)+s(x1, x2)+s(x3), data=dat, method="REML")
#' a <- getViz(b)
#' identical(b, getGam(a))
#' @rdname getGam
#' @export getGam
getGam <- function(o){
  
  if( !("gamViz" %in% class(o)) ){ stop("\"o\" should be of class \"gamViz\"") }
  
  o$store <- NULL
  class(o) <- class(o)[ -(which(class(o) == "gamViz"))] 
  
  return( o )
}