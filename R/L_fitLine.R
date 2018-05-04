#'
#' Add fitted smooth effect curve
#' 
#' @description This layes add lines representing a single or a group of 
#'              parametric or smooth 1D effects.
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_line}.
#' @return An object of class \code{gamLayer}.
#' @seealso See [plot.mgcv.smooth.1D], [plot.ptermNumeric], or
#'          [plot.fs.interaction.1D] for examples.
#' @details When used in conjuction with [plot.fs.interaction.1D], which plots smooth effects
#'          of type \code{bs="fs"}, this function uses transparency to avoid over-plotting.
#'          This can be avoided by setting \code{alpha = 1} in the call to \code{l_fitLine}.
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
l_fitLine.1D <- l_fitLine.PtermNumeric  <- l_fitLine.Multi1D <- function(a){
  
  a$data <- a$data$fit
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE}
  a$mapping <- aes("x" = x, "y" = ty)
  out <- do.call("geom_line", a)
  return( out )
  
}

######## Internal method 
#' @noRd
l_fitLine.randomEffect <- function(a){
  
  a$y <- a$data$fit$y
  a$data <- NULL
  
  out <- do.call("qqline", a)
  return( out )
  
}

######## Internal method 
#' @noRd
l_fitLine.MultiRandomEffect <-  function(a){
  
  # Need use only data from one of the quantile, otherwise l_fitLine.randomEffect thinks
  # that we have n * number_of_quantiles responses, rather than just n
  a$data$fit <- a$data$fit[a$data$fit$qu == levels(a$data$fit$qu)[1], ]

  return( l_fitLine.randomEffect(a) )
  
}



######## Internal method 
#' @noRd
l_fitLine.fs1D <- function(a){
  
  a$data <- a$data$fit
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE}
  if (is.null(a$alpha)){
    nf <- length( levels(a$data$id) ) # number of curves
    a$alpha <- c(1, 0.5, 0.3)[ findInterval(nf, c(0, 10, 100))  ]
  }
 
  out <- do.call("geom_line", a)
  return( out )
  
}

