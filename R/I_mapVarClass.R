
# Extract class of variable
.mapVarClass <- function(.cl){
  if( "integer" %in% .cl ){ return("numeric") }
  if( "numeric" %in% .cl ){ return("numeric") }
  if( "logical" %in% .cl ){ return("logical") }
  if( "factor" %in% .cl || "character" %in% .cl ){ return("factor") }
  return(.cl) # Not covered by mgcViz
}