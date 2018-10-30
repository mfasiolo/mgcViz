#'
#' Plotting parametric interactions
#' 
#' @description This function is here only to deal with parametric interactions (eg x0:fact), which
#'              cannot be plotted at the moment.
#' @name plot.ptermInteraction
#' @param x a parametric interaction object, extracted using [mgcViz::pterm].
#' @param ... currently unused.
#' @return Currently it returns \code{NULL}.
#' @rdname plot.ptermInteraction
#' @export plot.ptermInteraction
#' @export
#' 
plot.ptermInteraction <- function(x, ...){
  
  message("mgcViz does not know how to plot interactions. Returning NULL.")
  return( invisible(NULL) ) 
  
}

