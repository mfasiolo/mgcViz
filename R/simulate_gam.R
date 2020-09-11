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
#' @param trans function used to transform or summarize each vector of simulated responses. 
#'              It must take a vector as argument, but it can output a vector or a scalar.
#'              Potentially useful for saving storage (e.g. by transforming each simulated vector
#'              to a scalar). If left to \code{NULL} then \code{trans = identity} will be used.
#' @param newdata Optional new data frame or list to be passed to \link{predict.gam}.
#' @param u a matrix where each row is a vector of uniform random variables in (0, 1).
#'          This will be used to simulate responses only if \code{method = "qf"}. 
#' @param w vector of prior weights to be used in the simulations. If \code{newdata==NULL} then
#'          \code{w} is set to \code{object$prior.weights} otherwise it is a vector of ones.
#' @param offset numeric vector of offsets. For GAMs with multiple linear predictor (see eg \link{gaulss}) it
#'               must be a list of vectors. NB: if \code{newdata!=NULL} the offsets will be assumed to be zero, 
#'               unless their are explicitly provided. If \code{newdata==NULL} then simulations will use the 
#'               offsets used during model fitting, and \code{offset} argument will be ignored. 
#' @param ... extra arguments passed to \code{predict.gam}.
#' @return A matrix where each column is a vector of simulated responses. The number of rows
#'         is equal to the number of responses in the fitted object.
#' @examples 
#' library(mgcViz)
#' 
#' set.seed(2) ## simulate some data... 
#' dat <- gamSim(1,n=400,dist="normal",scale=2)
#' b <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
#' 
#' # Simulate three vectors of responses
#' matplot(simulate(b, nsim = 3), pch = 19, col = c(1, 3, 4)) 
#'
#' @importFrom plyr raply aaply laply
#' @export simulate.gam
#' @export
#' 
simulate.gam <- function(object, nsim = 1, seed = NULL, method = "auto", newdata, 
                         u = NULL, w = NULL, offset = NULL, trans = NULL, ...)
{
  o <- object
  method <- match.arg(method, c("auto", "rd", "qf"))
  if ( is.null(o$sig2) ){ o$sig2 <- summary(o)$dispersion }
  if( is.null(trans) ) { trans <- identity }
  
  # Either (a) use data in GAM object or (b) predict using new data 
  if( missing(newdata) ){ # (a) the offset should already be included in o$fitted.values
    
    mu <- o$fitted.values
    if( is.null(w) ) { w <- o$prior.weights }
    if( !is.null(offset) ) { message("simulate.gam: offset argument ignored. No newdata provided, so offset is already in object$fitted.values")}
    
  } else{ # (b) the user-defined offset is added to linear predictor
    
    mu <- predict(o, newdata = newdata, type = "link", ...)
    if( is.null(w) ){ w <- mu*0 + 1 }
    
    # Dealing with offset and inverting link function
    form <- o$formula
    if( is.list(form) ){ # [1] GAMLSS case
      n <- length( mu[[1]] )
      nte <- length( form ) 
      lnki <- lapply(o$family$linfo, "[[", "linkinv")
      if( is.null(offset) ) { offset <- rlply(nte, { numeric(n) }) }
      mu <- t( laply(1:nte, function(.ii){ lnki[[.ii]](mu[ , .ii] + offset[[.ii]]) }) )
    } else { # [2] GAM case
      if( is.null(offset) ) { offset <- mu * 0 }
      mu <- o$family$linkinv( mu + offset )
    }
    
  } 
  
  # Special cases: return straight away
  if(o$family$family == "Multivariate normal"){
    out <- rlply(nsim, function(.nouse) rmvn(nrow(mu), mu, solve(crossprod(o$family$data$R))))
    return( out )
  }
   
  out <- .simulate.gam(mu = mu, w = w, sig = o$sig2, method = method, fam = o$family, 
                         nsim = nsim, u = u, trans = trans)
  
  # We want nsim rows and number of columns depending on trans() 
  if( is.vector(out) ) { 
    out <- as.matrix( out )
    if(nsim == 1) { out <- t(out) }
  }
  
  return( t(unname(out)) )
  
}

# Internal function to simulate observations for given family
#
.simulate.gam <- function(mu, w, sig, method, fam, nsim, u, trans) {
  
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
    sim <- raply(nsim, { trans(fam$rd(mu, w, sig)) }  ) 
  }
  
  if( method == "qf" ){
    if( is.null(fam$qf) ) { fam <- fix.family.qf(fam) }
    if( is.null(fam$qf) ) { stop( "fam$qf unavailable, try using method = `rd`") }
    
    nobs <- length( mu )
    sim <- if( is.null(u) ){
      raply(nsim, { 
        trans(fam$qf(runif(nobs), mu, w, sig))
      }  ) 
    } else {
      aaply(u, 1, function(.u) { trans(fam$qf(.u)) }, mu = mu, wt = w, scale = sig)
    }
  }
  
  return( sim )
  
}