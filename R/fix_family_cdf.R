#'
#' Getting the CDF of a gam family
#' 
#' @description Some methods implemented in \code{mgcViz} require the c.d.f. of the response distribution.
#'              This function takes a family object as input and returns the same object, but with the cdf
#'              function added to the \code{$cdf} slot. Mainly for internal use.
#' @param fam an object of class \code{family}. 
#' @name fix.family.cdf
#' @importFrom stats ppois pbinom pgamma pnorm
#' @rdname fix.family.cdf
#' @export fix.family.cdf
#'
fix.family.cdf <- function(fam) {
  ## add cdf to family object
  
  if( !inherits(fam, "family") ){ stop("fam not a family object") }
  
  if( is.null(fam$cdf) ){
    
    fam$cdf <- switch(fam$family,
                      poisson = function(q, mu, wt, scale, logp = FALSE) {
                        ppois(q, mu, log.p=logp)
                      },
                      binomial = function(q, mu, wt, scale, logp = FALSE) {
                        pbinom(q*(wt + as.numeric(wt==0)), wt, mu, log.p=logp)
                      }, 
                      Gamma = function(q, mu, wt, scale, logp = FALSE) {
                        pgamma(q, shape=1/scale, scale=mu*scale, log.p=logp)
                      }, 
                      gaussian = function(q, mu, wt, scale, logp = FALSE) {
                        pnorm(q, mean=mu, sd=sqrt(scale/wt), log.p=logp)
                      })
  }
  
  return( fam )
}