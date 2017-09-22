#'
#' Add residuals to smooth effect plot
#' 
#' @description XXX
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_point}.
#' @return an object of class \code{gamLayer}.
#' @export resPoints
#'
resPoints <- function(...){
  arg <- list(...)
  o <- structure(list("fun" = "resPoints",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method 
#' @noRd
resPoints.plotSmooth1D <- function(a){
  
  a$data <- a$data$res[a$data$res$sub, ]
  a$mapping <- aes(x = x, y = y)
  a$inherit.aes <- FALSE
  if( is.null(a$shape) ) { a$shape <- 46 } 
  if( is.null(a$na.rm) ) { a$na.rm <- TRUE } 
  
  if( !is.null(a$data$y) ){
    fun <- "geom_point"
    out <- do.call(fun, a)
  } else {
    message("resPoints(): Partial residuals are not available") 
    out <- NULL
  }
  return( out )
}

######## Internal method 
#' @noRd
resPoints.plotSmooth2D <- function(a){
  
  return( resPoints.plotSmooth1D(a) )
  
}