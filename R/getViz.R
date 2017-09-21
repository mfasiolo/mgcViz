#' Convert gamObject to gamViz object
#' 
#' @description XXX
#' @name getViz
#' @examples 
#' library(mgcViz)
#' set.seed(2) ## simulate some data...
#' dat <- gamSim(1,n=1000,dist="normal",scale=2)
#' b <- gam(y~s(x0)+s(x1, x2)+s(x3), data=dat, method="REML")
#' b <- getViz(b)
#'
#' plot(sm(b,1)) + fitLine() + ciLine() + resRug() + resPoints()
#' plot(sm(b,2), rug = T, residuals = T, scheme=2)
#' @rdname getViz
#' @export getViz
getViz <- function(o){
  
  if( !("gam" %in% class(o)) ){ stop("\"o\" should be of class \"gam\"") }
  
  tmp <- o$pterms
  np <- if (is.list(tmp)){ length(unlist(lapply(tmp,attr,"order"))) } else { length(attr(tmp,"order")) }
  o$store <- list("termsFit"=predict(o, type = "terms"), "np"=np)
  rm(list=c("tmp"))
  
  class(o) <- c("gamViz", class(o))
  
  return( o )
}

