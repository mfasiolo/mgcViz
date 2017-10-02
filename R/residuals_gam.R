#' Generalized Additive Model residuals
#' 
#' @description Wrapper around \code{mgcv::residuals.gam}.
#' @name residuals.gam
#' @rdname residuals.gam
#' @export residuals.gam
residuals.gam <- function(object, type = "deviance", ...)
{
  
  type <- match.arg(type, c("deviance", "pearson", "scaled.pearson", "working", "response", "tunif", "tnormal"))
  
  o <- object

  if( type %in% c("tunif", "tnormal") ){

    fam <- fix.family.cdf( o$family )
    
    if( is.null(fam$cdf) ){ stop("CDF unavailable for this family: can't use `tunif` or `tnormal` type") }
     
    y <- fam$cdf(o$y, o$fitted.values, o$prior.weights, o$sig2, logp = TRUE)
    
    if( type == "tnormal" ) { y <- qnorm(y, log.p = TRUE) } else { y <- exp(y) } 
    
  } else {
    
    # NB `mgcv::` important to avoid an infinite recursion
    y <- mgcv::residuals.gam(o, type = type, ...) 
    
  }
  
  return( y )
  
}