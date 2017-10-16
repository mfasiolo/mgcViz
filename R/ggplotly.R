
#############################################
#' Convert to plotly
#' 
#' @param x An object of class \code{plotSmooth}.
#' @name ggplotly.plotSmooth
#' @importFrom plotly ggplotly
#' @rdname ggplotly.plotSmooth
#' @export ggplotly.plotSmooth
#'
ggplotly.plotSmooth <- ggplotly.qqGam <- function(p, ...){
  
  ggplotly( p$ggObj )
  
}


#' Convert to plotly
#' 
#' @param x An object of class \code{qqGam}.
#' @name ggplotly.qqGam
#' @rdname ggplotly.qqGam
#' @export ggplotly.qqGam
#'
ggplotly.qqGam <- function(p, ...){
  
  ggplotly( p$ggObj )
  
}