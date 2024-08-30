
# Returns the appropriate residual type for each GAM family

.getResTypeAndMethod <- function(fam){
  
  type <- "deviance"
  method <- "simul1"
  
  if( !is.null(fam$cdf) ){
    type <- "tnormal"
    method <- "tnormal"
  }

  return( list("type" = type, "method" = method) )
          
}