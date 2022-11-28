################################################################
#' Converting gam objects to gamViz objects
#' 
#' @description This function converts \code{gam} objects into \code{gamViz} objects, 
#'              for which \code{mgcViz} provides several plotting methods.
#' @param o an object of class \code{gam}.
#' @param nsim the number of simulated vectors of responses. A positive integer.
#' @param post if \code{TRUE} then posterior simulation is performed. That is, we simulate \code{nsim} vectors 
#'             of regression coefficients from a Gaussian approximation to the posterior, and then we simulate
#'             a vector of response using each parameter vector. If \code{FALSE}, then \code{nsim} vectors of 
#'             responses are simulated using parameters fixed at the posterior mode. 
#' @param newdata Optional new data frame used to perform the simulations. To be passed to \link{predict.gam} and, 
#'                if \code{post == TRUE}, to \code{postSim}.
#' @param ... extra arguments to be passed to \link{simulate.gam} (if \code{post==FALSE}) or
#'            \link{postSim} (if \code{post==TRUE}). For instance, we could pass prior 
#'            weights \code{w} and \code{offset}.
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
getViz <- function(o, nsim = 0, post = FALSE, newdata, ...){

  if( inherits(o, "list") ){
    tmp <- names(o)
    # lapply does not handle missing arguments in the "...", need to pass it explictly
    o <- lapply(o, function(.x, newdata, ...) getViz(.x, nsim = nsim, post = post, newdata = newdata, ...), newdata = newdata)
    if( is.null(tmp) ) names(o) <- 1:length(o)
    class(o) <- "mgamViz"
    return( o )
  }
  
  if( inherits(o, "mqgam") ){
    qus <- as.numeric( names(o$fit) )
    cal <- o$calibr
    o <- qdo(o, qus, getViz, nsim = 0, post = post, newdata = newdata, ...)
    names(o) <- qus
    # Need to add calibration information for each QGAM
    for(ii in 1:length(qus)){ 
      o[[ii]]$calibr <- list("lsig" = cal$lsig[ii], 
                             "err" = cal$err[ii], 
                             "ranges" = matrix(cal$ranges[ii, ], nrow = 1), 
                             "store" = cal$store[ii])
      attr(o[[ii]]$calibr, "class") <- attr(cal, "class")
    }
    class(o) <- c("mqgamViz", "mgamViz")
    return( o )
  }
  
  if( !inherits(o, "gam") ){ stop("\"o\" should be of class \"gam\"") }
  
  # If `o` is already a `gamViz` object we don't recompute the `termsFit`
  if( !inherits(o, "gamViz") ){
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
  # NB: we do not allow to use trans() here, as it might lead to problems with check1D and check2D
  if( nsim > 0 ){ 
    if( post ){ # Posterior simulations OR ...
      tryCatch(o$store$sim <- postSim(o, nsim = nsim, newdata = newdata, trans = NULL, savePar = FALSE, ...), 
               error = function(e){ 
                 message( paste("postSim() failed:", e$message) ) 
               })
    } else {    # ... parameters fixed at MAP 
      tryCatch(o$store$sim <- simulate(o, nsim = nsim, newdata = newdata, trans = NULL, ...), 
               error = function(e){ 
                 message( paste("simulate.gam() failed:", e$message) ) 
               })
    }
  }
  
  # We store new dataset which will be used for checking in check0D, check1D, check2D ...
  # If object 'o' already contained newdata, we either over-write it or set it to NULL
  if( !missing(newdata) ){
    o$store$newdata <- newdata
  } else {
    if( !is.null(o$store$newdata) ) { message("getViz: newdata removed from gamViz object") }
    o$store$newdata <- NULL 
  }
  
  return( o )
}

