#'
#' Simulating responses from a GAM object
#' 
#' @description This method can be used to simulate vectors of responses from a gamObject.
#' 
#' @param object the output of a \code{gam()} or \code{bam()} call.
#' @param nsim the number of simulated vectors of responses. A positive integer.
#' @param seed currently not used.
#' @param method the method used for the simulation. If set to "rd" then \code{o$family$rd()}
#'               will be used, if available. If set to "qf" then \code{o$family$qf()} (which is
#'               the inverse cdf of the response distribution) will be used to transform some
#'               uniform variates.
#' @param u a matrix where each row is a vector of uniform random variables in (0, 1).
#'          This will be used to simulate responses only if \code{method = "qf"}. 
#' @param ... currently not used.
#' @return A matrix where each row is a vector of simulated responses. The number of columns
#'         is equal to the number of responses in the fitted object.
#' @examples 
#' library(mgcViz)
#' 
#' set.seed(2) ## simulate some data... 
#' dat <- gamSim(1,n=400,dist="normal",scale=2)
#' b <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
#' 
#' # Simulate three vectors of responses
#' matplot(t(simulate(b, nsim = 3)), pch = 19, col = c(1, 3, 4)) 
#'
#' @importFrom plyr raply aaply
#' @export simulate.gam
#' @export
#' 
simulate.gam <- function(object, nsim = 1, seed = NULL, method = "auto", u = NULL,  ...)
{
  o <- object
  method <- match.arg(method, c("auto", "rd", "qf"))
  if ( is.null(o$sig2) ){ o$sig2 <- summary(o)$dispersion }
  
  fam <- o$family
  
  # Try method == 'rd', if that does not work 'qf', if that is not good either throw an error
  if( method == "auto" ){
    fam <- fix.family.rd(fam)
    if( is.null(fam$rd) ) { 
      fam <- fix.family.qf(fam) 
      method <- 'qf'
    } else { 
      method <- "rd" 
    }
    if( method=="qf" && is.null(fam$qf) ) { stop("No simulation method available for this family")  } 
  }
  
  if( method == "rd" ){
    if( is.null(fam$rd) ) { fam <- fix.family.rd(fam) }
    if( is.null(fam$rd) ) { stop( "fam$rd unavailable, try using method = `qf`") }
    sim <- raply(nsim, { fam$rd(o$fitted.values, o$prior.weights, o$sig2) }  ) 
  }
  
  if( method == "qf" ){
    if( is.null(fam$qf) ) { fam <- fix.family.qf(fam) }
    if( is.null(fam$qf) ) { stop( "fam$qf unavailable, try using method = `rd`") }
    
    nobs <- length( o$fitted.values )
    sim <- if( is.null(u) ){
      raply(nsim, { 
        fam$qf(runif(nobs), o$fitted.values, o$prior.weights, o$sig2)
      }  ) 
    } else {
      aaply(u, 1, fam$qf, mu = o$fitted.values, wt = o$prior.weights, scale = o$sig2)
    }
  }
  
  if( is.vector(sim) ) { sim <- matrix(sim, 1, length(sim)) }
  
  return( sim )
  
}