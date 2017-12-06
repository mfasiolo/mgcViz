#'
#' Add polygons to effect plots
#' 
#' @description This layers adds polygons to plots and it is mainly usefuls for
#'              plotting Markov random field smooths.
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_polygon}.
#' @return An object of class \code{gamLayer}.
#' @seealso See [plot.mrf.smooth] for examples.
#' @export l_poly
#'
l_poly <- function(...){
  arg <- list(...)
  o <- structure(list("fun" = "l_poly",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}


######## Internal method 
#' @noRd
l_poly.mrf <- function(a){
  
  xtra <- a$xtra
  a$xtra <- NULL
  
  if( is.null(a$colour) ) { a$colour <- "white" }
  a$data <- a$data$fit
  
  out <- list()
  out[[1]] <- do.call("geom_polygon", a)
  out[[2]] <- scale_fill_gradientn(colours = viridis(50, begin = 0.2), 
                                   na.value = "grey", name = "s(x)") 

  class(out) <- "listOfLayers"
  
  return( out )
}



