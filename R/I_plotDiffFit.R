############### Internal function
#' @noRd
#' 
.plotDiffFit <- function(sm, gObj, n, ...){
  
  crange <- seq(sm$first.para, sm$last.para)
  edf   <- sum( gObj$edf[crange] ) ## Effective DoF for this term
  term.lab <- .subEDF(sm$label, edf)
  attr(sm, "coefficients") <- gObj$coefficients[crange] # Relevant coeffs for the smooth
  P <- .prepare(x = sm, data = gObj$model, n = n, n2 = n, label = term.lab, ...)
  
  # Get fitted values ....
  p <- gObj$coefficients[ crange ]   ## relevant coefficients 
  offset <- attr(P$X, "offset")      ## any term specific offset
  if (is.null(offset)) {
    P$fit <- P$X %*% p
  } else {
    P$fit <- P$X %*% p + offset 
  }
  
  P$crange <- crange
  
  return( P )
  
}