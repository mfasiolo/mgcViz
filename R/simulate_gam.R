
#' Simulating responses from a GAM object
#' @description ...
#' @param nsim the number of simulated datasets. A positive integer.
#' @param u a matrix where each row is a vector of uniform random variables in (0, 1).
#' @return a matrix where each row is a vector of simulated responses.
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
  
  return( sim )
  
}