#'
#' Add theme to smooth effects plot
#' 
#' @description This is essentially an adaptor or wrapper, useful for making
#'              \code{ggplot2} themes work with \code{mgcViz} plots.
#'
#' @param th A theme object, produced using \code{ggplot2}.
#' @return An object of class \code{gamTheme}.
#' @export wrapTheme
#' @examples 
#'
wrapTheme  <- function(th){
  if( !("theme" %in% class(th)) ) { stop("`th` should be of class `theme`") }
  class(th) <- "gamTheme"
  return(th)
} 