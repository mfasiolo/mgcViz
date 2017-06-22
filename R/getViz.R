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
#' sm1 <- b(1)
#' sm2 <- b(2)
#' 
#' plot(sm1, rug = T, residuals = T)
#' plot(sm2, rug = T, residuals = T, scheme=2)
#' @rdname getViz
#' @export getViz
getViz <- function(o){

  if( !("gam" %in% class(o)) ){ stop("\"o\" should be of class \"gam\"") }

  gObj <- o
  
  tmp <- gObj$pterms
  np <- if (is.list(tmp)){ length(unlist(lapply(tmp,attr,"order"))) } else { length(attr(tmp,"order")) }
  store <- list("termsFit"=predict(gObj, type = "terms"), "np"=np)
  
  rm(list=c("o", "tmp", "np"))

  .smoothExtractor <- function(select){

    m <- length(gObj$smooth) # number of smooth effects

    if(length(select)>1){ stop("select should be a scalar") }
    if(select > m){ stop(paste("select should be smaller than", m, "the number of smooths in gamObject")) }

    out <- list("ism"=select, "store"=store, "gObj"=gObj)

    cl <- class( gObj$smooth[[select]] )

    if("mgcv.smooth" %in% cl){
      cl[which(cl=="mgcv.smooth")] <- paste("mgcv.smooth.", gObj$smooth[[select]]$dim, "D", sep='')
      class(out) <- cl
    }

    return( out )

  }
  
  class(.smoothExtractor) <- c("gamViz")

  return( .smoothExtractor )
}

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

  out <- get("gObj", envir = environment(o))
  out$termsFit <- NULL

  return( out )
}


# library(mgcv)
# set.seed(2) ## simulate some data...
# dat <- gamSim(1,n=1000,dist="normal",scale=2)
# b <- gam(y~s(x0)+s(x1, x2)+s(x3), data=dat, method="REML")
# b <- getViz(b)
# sm1 <- b(1)
# sm2 <- b(2)
# 
# plot(sm1, rug = T, residuals = T)
# plot(sm2, rug = T, residuals = T, scheme=2)
# 
# # library(gridExtra);
# # grid.arrange(grobs = list( plot(sm1, rug = T, residuals = T), 
#                            plot(sm2, rug = T, residuals = T, scheme=2)), ncol = 2)



