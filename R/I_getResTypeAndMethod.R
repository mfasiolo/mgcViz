
# Returns the appropriate residual type for each GAM family

.getResTypeAndMethod <- function(fam){
  
  type <- switch(fam, 
                 "shash" = "tnormal", 
                 "gaulss" = "deviance",
                 "deviance") # Default
  
  method <- switch(fam, 
                   "shash" = "tnormal",
                   "gaulss" = "normal",
                   "simul1") # Default
  
  return( list("type" = type, "method" = method) )
          
}