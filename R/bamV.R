########
## Fit a GAM model with bam and get a gamViz object
#' @rdname gamV
#' @export bamV
#
bamV <- function(formula, family = gaussian(), data = list(), method = "fREML", aGam = list(), aViz = list()){
  
  obj <- do.call("bam", c(list("formula" = formula, "family" = family, "data" = quote(data), "method" = method), aGam))
  
  obj <- do.call("getViz", c(list("o" = obj), aViz))
  
  return( obj )
  
}


