#' Convert gamViz object to gamObject 
#' 
#' @description Function for converting a \code{gamViz} object to a \code{gamObject}.
#'              It is essentially the inverse of the [getViz] function.
#'              
#' @param o a \code{gamViz} object, the output of [mgcViz::getViz].
#' @name getGam
#' @examples 
#' library(mgcViz)
#' set.seed(2) ## simulate some data...
#' dat <- gamSim(1,n=1000,dist="normal",scale=2)
#' b <- gam(y~s(x0)+s(x1, x2)+s(x3), data=dat, method="REML")
#' a <- getViz(b)
#' identical(b, getGam(a)) # Must be TRUE
#' @rdname getGam
#' @export getGam
getGam <- function(o){
  
  if( !("gamViz" %in% class(o)) ){ stop("\"o\" should be of class \"gamViz\"") }
  
  o$store <- NULL
  class(o) <- class(o)[ -(which(class(o) == "gamViz")) ] 
  
  return( o )
  
}