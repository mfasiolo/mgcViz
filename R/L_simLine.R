#'
#' Add simulated smooth effect curves
#' 
#' @description This layer adds curves representing smooth effects simulated from the
#'              posterior distribution.
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_line}.
#' @return An object of class \code{gamLayer}.
#' @seealso See [plot.mgcv.smooth.1D] for examples.
#' @details This function uses transparency to avoid over-plotting.
#'          This can be avoided by setting \code{alpha = 1} in the call to \code{l_simLine}.
#' @export l_simLine
#' 
l_simLine <- function(...){
  arg <- list(...)
  o <- structure(list("fun" = "l_simLine",
                      "arg" = arg), 
                 class = c("gamLayer"))
  return(o)
}

######## Internal method 
#' @noRd
l_simLine.1D <- function(a){
  
  a$data <- a$data$sim
  
  if( is.null(a$data) ){
    message("l_simLine(): the object does not contain any posterior simulation.")
    return( NULL )
  }
  
  if( is.null(a$mapping) ){ 
    a$mapping <- aes("x" = x, "y" = ty, "group" = id) 
    if( is.null(a$colour) ){ a$colour <- "grey40" }
  } else{
    if(is.null(a$mapping$colour)){ a$colour <- "grey40" }
  }
  
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE}
  if (is.null(a$alpha)){
    nf <- length( levels(a$data$id) ) # number of curves
    a$alpha <- c(0.5, 0.3, 0.2, 0.1)[ findInterval(nf, c(0, 10, 50, 100))  ]
  }
  
  out <- do.call("geom_line", a)
  return( out )

}

