#' 
#' @description Add boundaries to smooth plot
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_path}.
#' @return An object of class \code{gamLayer}.
#' @export l_bound
#'
l_bound <- function(n = 200, ...){
  arg <- list(...)
  arg$xtra <- list("n" = n)
  o <- structure(list("fun" = "l_bound",
                      "arg" = arg), 
                 class = c("gamLayer"))
  return(o)
}

######## Internal method 
#' @noRd
l_bound.plotSmoothsos0gg <- function(a){
  
  n <- a$xtra$n
  a$xtra <- NULL
  
  theta <- seq(-pi/2, pi/2, length = n)
  x <- sin(theta) 
  y <- cos(theta)
  
  a$data <- data.frame("x" = c(x, rev(x)), "y" = c(y, -rev(y)))
  a$inherit.aes <- FALSE
  a$mapping <- aes(x = x, y = y)
  
  fun <- "geom_path"
  out <- do.call(fun, a)
  
  return( out )
  
}
