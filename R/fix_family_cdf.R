

fix.family.cdf <- function(fam) {
  ## add cdf to family object
  
  if( !inherits(fam, "family") ){ stop("fam not a family object") }
  
  if( is.null(fam$cdf) ){
    
    fam$cdf <- switch(fam$family,
                      poisson = function(q, mu, wt, scale) {
                        ppois(q, mu)
                      },
                      binomial = function(q, mu, wt, scale) {
                        pbinom(q*(wt + as.numeric(wt==0)), wt, mu)
                      }, 
                      Gamma = function(q, mu, wt, scale) {
                        pgamma(q, shape=1/scale, scale=mu*scale)
                      }, 
                      gaussian = function(q, mu, wt, scale) {
                        pnorm(q, mean=mu, sd=sqrt(scale/wt))
                      })
  }
  
  return( fam )
}