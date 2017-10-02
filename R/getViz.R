################################################################
#' Converting gam objects to gamViz objects
#' 
#' @description This function converts \code{gam} objects into \code{gamViz} objects.
#' @param o an object of class \code{gam}.
#' @param nsim the number of simulated vectors of responses. A positive integer.
#' @param ... extra arguments to be passed to \code{simulate.gam}
#' @return An object of class \code{gamViz}.
#' @name getViz
#' @examples 
#' library(mgcViz)
#' set.seed(2) ## simulate some data...
#' dat <- gamSim(1,n=1000,dist="normal",scale=2)
#' b <- gam(y~s(x0)+s(x1, x2)+s(x3), data=dat, method="REML")
#' b <- getViz(b)
#' str(b$store$sim) # Simulated responses now stored here
#'
#' plot(sm(b,1)) + l_fitLine() + l_ciLine() + l_rug() + l_points()
#' plot(sm(b,2)) + l_rug() + l_fitRaster() + l_fitContour()
#' @rdname getViz
#' @export getViz
getViz <- function(o, nsim = 10, ...){
  
  if( !("gam" %in% class(o)) ){ stop("\"o\" should be of class \"gam\"") }
  
  # If `o` is already a `gamViz` object we don't recompute the `termsFit`
  if( !("gamViz" %in% class(o)) ){
    
    tmp <- o$pterms
    np <- if (is.list(tmp)){ length(unlist(lapply(tmp,attr,"order"))) } else { length(attr(tmp,"order")) }
    o$store <- list("termsFit"=predict(o, type = "terms"), "np"=np)
    rm(list=c("tmp"))
    
    class(o) <- c("gamViz", class(o))
  }
  
  if( nsim > 0 ){ o$store$sim <- simulate(o, n = nsim, ...) }
  
  return( o )
}

