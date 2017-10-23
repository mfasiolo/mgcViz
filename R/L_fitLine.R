#'
#' Add fitted smooth effect curve
#' 
#' @description XXX
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_line}.
#' @return An object of class \code{gamLayer}.
#' @export l_fitLine
#'
l_fitLine <- function(...){
  arg <- list(...)
  o <- structure(list("fun" = "l_fitLine",
                      "arg" = arg), 
                 class = c("gamLayer"))
  return(o)
}

######## Internal method 
#' @noRd
l_fitLine.plotSmooth1Dgg <- function(a){
  
  a$data <- a$data$fit
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE}
  a$mapping <- aes("x" = x, "y" = ty)
  fun <- "geom_line"
  out <- do.call(fun, a)
  return( out )
  
}

######## Internal method 
#' @noRd
l_fitLine.plotSmoothfs1Dgg <- function(a){
  
  a$data <- a$data$fit
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE}
  if (is.null(a$alpha)){
    nf <- length( levels(a$data$id) ) # number of curves
    a$alpha <- c(1, 0.5, 0.3)[ findInterval(nf, c(0, 10, 100))  ]
  }
 
  a$mapping <- aes("x" = x, "y" = ty, "colour" = id)  
  out <- do.call("geom_line", a)
  return( out )
  
}

