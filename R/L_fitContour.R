#'
#' Add fitted smooth effect contour
#' 
#' @description XXX
#'
#' @param ... graphical arguments to be passed to  \code{ggplot2::geom_contour}.
#' @return An object of class \code{gamLayer}.
#' @export l_fitContour
#'
l_fitContour <- function(...){
  arg <- list(...)
  o <- structure(list("fun" = "l_fitContour",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method 
#' @noRd
l_fitContour.plotSmooth2Dgg <- function(a){
  
  a$data <- a$data$fit
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE}
  if( is.null(a$colour) ){ a$colour <- "black" }
  fun <- "geom_contour"
  out <- do.call(fun, a)
  return( out )
  
}

######## Internal method 
#' @noRd
l_fitContour.plotSmoothfs1Dgg <- function(a){
  
  a$data <- a$data$fit
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE}
  if (is.null(a$alpha)){
    nf <- length( levels(a$data$id) ) # number of curves
    a$alpha <- c(1, 0.5, 0.3)[ findInterval(nf, c(0, 10, 100))  ]
  }
  
  a$mapping <- aes("x" = x, "y" = y, "colour" = id)  
  out <- do.call("geom_line", a)
  return( out )
  
}