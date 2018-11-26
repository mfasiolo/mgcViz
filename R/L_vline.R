#'
#' Adding vertical line to a plot
#' 
#' @description This layer adds a vertical to a plot. It is mainly a 
#'              wrapper around [ggplot2::geom_vline]. 
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_vline}.
#' @return An object of class \code{gamLayer}.
#' @seealso See [check0D] for examples.
#' @export l_vline
#'
l_vline <- function(...){
  arg <- list(...)
  o <- structure(list("fun" = "l_vline",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method for posterior checks: scalar case
#' @noRd
l_vline.Check0DScalarNumeric <- function(a){
  
  a$data <- data.frame("x" = a$data$res$x)
  
  if( is.null(a$mapping) ) { 
    a$mapping <- aes("xintercept" = x)
  }
  
  if( is.null(a$colour) ){ a$colour <- "red" }
  
  fun <- "geom_vline"
  out <- do.call(fun, a)
  
  return( out )
  
}