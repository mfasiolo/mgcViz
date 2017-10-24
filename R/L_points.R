#'
#' Add residuals to smooth effect plot
#' 
#' @description XXX
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_point}.
#' @return an object of class \code{gamLayer}.
#' @export l_points
#'
l_points <- function(...){
  arg <- list(...)
  o <- structure(list("fun" = "l_points",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method 
#' @noRd
l_points.plotSmooth1Dgg <- l_points.plotSmoothsos1gg <- l_points.plotSmoothsos0gg <- 
                           l_points.plotSmoothCheck1Dgg <- function(a){
  
  a$data <- a$data$res[a$data$res$sub, ]
  a$mapping <- aes(x = x, y = y)
  a$inherit.aes <- FALSE
  if( is.null(a$shape) ) { a$shape <- 46 } 
  if( is.null(a$na.rm) ) { a$na.rm <- TRUE } 
  
  if( !is.null(a$data$y) ){
    fun <- "geom_point"
    out <- do.call(fun, a)
  } else {
    message("l_points(): Partial residuals are not available") 
    out <- NULL
  }
  return( out )
}

######## Internal method 
#' @noRd
l_points.plotSmooth2Dgg <- l_points.plotSmoothCheck2Dgg <- function(a){
  
  return( l_points.plotSmooth1Dgg(a) )
  
}

