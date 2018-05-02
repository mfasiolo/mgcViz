################################################################
#' Converting gam objects to gamViz objects
#' 
#' @description This function converts \code{gam} objects into \code{gamViz} objects, 
#'              for which \code{mgcViz} provides several plotting methods.
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
#' b <- getViz(b, nsim = 20)
#' str(b$store$sim) # Simulated responses now stored here
#'
#' plot(sm(b,1)) + l_fitLine() + l_ciLine() + l_rug() + l_points()
#' plot(sm(b,2)) + l_rug() + l_fitRaster() + l_fitContour()
#' @importFrom stats simulate
#' @importFrom qgam qdo
#' @rdname getViz
#' @export getViz
getViz <- function(o, nsim = 0, ...){
  
  if( "mqgam" %in% class(o) ){
    qus <- as.numeric( names(o$fit) )
    o <- lapply(qus, function(.q) qdo(o, .q, getViz, nsim = 0))
    names(o) <- qus
    class(o) <- "mgamViz"
    return( o )
  }
  
  if( !("gam" %in% class(o)) ){ stop("\"o\" should be of class \"gam\"") }
  
  # If `o` is already a `gamViz` object we don't recompute the `termsFit`
  if( !("gamViz" %in% class(o)) ){
    
    tmp <- o$pterms
    np <- if (is.list(tmp)){ length(unlist(lapply(tmp,attr,"order"))) } else { length(attr(tmp,"order")) }
    ns <- length( o$smooth )
    terms <- predict(o, type = "terms")
    
    # predict.bam with discrete = T does not predict parametric terms of order 2. Hence we need to add some empty columns.
    nmis <- (np + ns) - ncol(terms) 
    if( nmis ){
      M1 <- if(np - nmis) { terms[ , 1:(np-nmis)] } else { c() }
      M2 <- matrix(0, nrow(terms), nmis, 
                   dimnames = list(c(), paste(".fakVar", 1:nmis, sep = '') ))
      M3 <- if(ns) { terms[ , (ncol(terms)-ns+1):ncol(terms)] } else { c() }
      terms <- cbind(M1, M2, M3)
    }

    o$store <- list("termsFit"=terms, "np"=np)

    class(o) <- c("gamViz", class(o))
  }
  
  # We try to simulate responses. If an error occurs we report it but do no stop.
  # Most likely error if that o$family does not have any simulation method available.
  if( nsim > 0 ){ 
    tryCatch(o$store$sim <- simulate(o, nsim = nsim, ...), 
             error = function(e){ 
               message( paste("simulate.gam() failed:", e$message) ) 
             })
  }
  
  return( o )
}

