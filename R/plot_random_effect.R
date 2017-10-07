#' Plotting random effects
#' 
#' @description XXX Does not work yet.
#' @name plot.random.effect
#' @rdname plot.random.effect
#' @export plot.random.effect
plot.random.effect <- function(...) {
  
  out <- list("ggObj" = ggplot(), "data" = NULL) 
  
  class(out) <- c("plotSmooth", "randomEffect", "gg")
  
  return(out)
}

