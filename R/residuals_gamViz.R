#'
#' Generalized Additive Model residuals
#' 
#' @description Extension of \code{mgcv::residuals.gam}. Returns residuals for a fitted GAM model object. 
#'              Pearson, deviance, working and response residuals are available as in the method from 
#'              \code{mgcv}, but this version also provides residual types "tunif" and "tnormal". The former are
#'              obtained using the cdf of the response distribution (if available). The latter are obtained
#'              by further transforming the uniform residuals using the quantile function (i.e. the inverse
#'              cdf) of a standard normal variable. 
#' @param object an object of class \code{gamViz}, the output of a \code{getViz()} call.
#' @param type the type of residuals wanted. If should be one of "deviance", "pearson", "scaled.pearson",
#'             "working", "response", "tunif" or "tnormal". Not all are available for each family.
#' @param ... further arguments passed to [mgcv::residuals.gam].
#' @name residuals.gamViz
#' @seealso See also [mgcv::residuals.gam] for details.
#' @rdname residuals.gamViz 
#' @importFrom mgcv residuals.gam
#' @export residuals.gamViz 
#' @export
#' 
residuals.gamViz <- function(object, type = "deviance", ...)
{
  
  if( !inherits(object, "gamViz") ){ stop("Argument 'object' should be of class 'gamViz'. See ?getViz") }
  
  type <- match.arg(type, c("deviance", "pearson", "scaled.pearson", "working", "response", "tunif", "tnormal"))
  
  o <- object

  if( type %in% c("tunif", "tnormal") ){

    fam <- fix.family.cdf( o$family )
    
    if( is.null(fam$cdf) ){ stop("CDF unavailable for this family: can't use `tunif` or `tnormal` type") }
     
    y <- fam$cdf(o$y, o$fitted.values, o$prior.weights, o$sig2, logp = TRUE)
    
    if( type == "tnormal" ) { y <- qnorm(y, log.p = TRUE) } else { y <- exp(y) } 
    
  } else {
    
    y <- residuals.gam(o, type = type, ...) 
    
  }
  
  return( y )
  
}