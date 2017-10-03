
#############################################
#' Convert to plotly
#' 
#' @param x An object of class \code{plotSmooth}.
#' @name ggplotly.plotSmooth
#' @importFrom plotly ggplotly
#' @rdname ggplotly.plotSmooth
#' @export ggplotly.plotSmooth
#'
ggplotly.plotSmooth <- function(p, ...){
  
  ggplotly( p$ggObj )
  
}