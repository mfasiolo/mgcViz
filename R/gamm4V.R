########
# Fit a GAMM or GAMM4 model and get a gamViz object
#' @rdname gammV
#' @export gamm4V
#

gamm4V <- function(formula, random, family = gaussian(), data = list(), REML = TRUE, aGam = list(), aViz = list(), keepGAMObj = FALSE){

  obj <- do.call("gamm4", c(list("formula" = formula, "random" = random, 
                                 "family" = family, "data" = quote(data), "REML" = REML), aGam))

  mer <- obj$mer

  obj <- do.call("getViz", c(list("o" = obj$gam), aViz))
  
  # Make sure that the stored function call refers to the name of the data set provided 
  # by the user to gamm4V (and available in environment where gamm4V was called), not just 
  # to "data" (as in the call to gamm4 via do.call)
  obj$call$data <- match.call()$data

  if ( keepGAMObj ) { obj$gam <- obj }
  obj$mer <- mer

  return( obj )

}
