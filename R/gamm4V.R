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

  if ( keepGAMObj ) { obj$gam <- obj }
  obj$mer <- mer

  return( obj )

}
