########
## Fit a GAM model with bam and get a gamViz object
#' @rdname gamV
#' @export bamV
#
bamV <- function(formula, family = gaussian(), data = list(), method = "fREML", aGam = list(), aViz = list()){
  
  obj <- do.call("bam", c(list("formula" = formula, "family" = family, "data" = quote(data), "method" = method), aGam))
  
  obj <- do.call("getViz", c(list("o" = obj), aViz))
  
  # Make sure that the stored function call refers to the name of the data set provided 
  # by the user to bamV (and available in environment where bamV was called), not just 
  # to "data" (as in the call to bam via do.call)
  obj$call$data <- match.call()$data
  
  return( obj )
  
}


