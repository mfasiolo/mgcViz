#'
#' Add coordinate lines
#' 
#' @description XXX
#'
#' @param ... graphical arguments to be passed to  \code{ggplot2::geom_contour}.
#' @return An object of class \code{gamLayer}.
#' @export l_coordContour
#'
l_coordContour <- function(brLO = c(-8:9*20), brLA = c(-8:8*10), ...){
  arg <- list(...)
  arg$xtra <- list("brLO" = brLO, "brLA" = brLA)
  o <- structure(list("fun" = "l_coordContour",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method 
#' @noRd
l_coordContour.plotSmoothsos0gg <- function(a){
  
  xtra <- a$xtra
  a$xtra <- NULL
  
  a$data <- a$data$fit
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE}
  if( is.null(a$colour) ){ a$colour <- "black" }
  if( is.null(a$linetype) ){ a$linetype <- 2 }
  fun <- "geom_contour"
  
  out <- list()
  a$mapping  <- aes(z = lo)
  a$breaks <- xtra$brLO
  out[[1]] <- do.call(fun, a)
  
  a$mapping  <- aes(z = la)
  a$breaks <- xtra$brLA
  out[[2]] <- do.call(fun, a)
  
  class(out) <- "listOfLayers"
  
  return( out )
  
}