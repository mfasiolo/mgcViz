#'
#' Adding points representing the fitted effect
#' 
#' @description This function adds points representing the fitted effect. Mainly
#'              useful for plotting factor effects.
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_point}.
#' @return an object of class \code{gamLayer}.
#' @seealso See [plot.ptermFactor] for examples.
#' @export l_fitPoints
#'
l_fitPoints <- function(...){
  arg <- list(...)
  o <- structure(list("fun" = "l_fitPoints",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method for factor plots
#' @noRd
#'
l_fitPoints.PtermFactor <- l_fitPoints.MultiPtermNumeric <- function(a){
  
  if( is.null(a$shape) ){ a$shape <- 19}
  if( is.null(a$size) ){ a$size <- 2}
  
  # Substituting residuals data with fit data, so we can use code for residuals points plot
  a$data$res <- a$data$fit
  a$data$res$y <- a$data$res$ty
  a$data$res$sub <- rep(TRUE, nrow(a$data$res))

  l_points.1D( a )
  
}

######## Method for mqgam fits
#' @noRd
#'
l_fitPoints.MultiPtermFactor <- function(a){
  
  if( is.null(a$mapping) ){ a$mapping <- aes("x" = id, "y" = ty, "colour" = id) }
  
  l_fitPoints.PtermFactor( a )
  
}
