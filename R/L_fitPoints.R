#'
#' Adding fitted points to effect plots
#' 
#' @description Adding fitted points to smooth or parametric effect plots.
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_point}.
#' @return an object of class \code{gamLayer}.
#' @export l_points
#'
l_fitPoints <- function(...){
  arg <- list(...)
  arg$xtra <- list("jit" = c(FALSE, FALSE))
  o <- structure(list("fun" = "l_fitPoints",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method for factor plots
#' @noRd
#'
l_fitPoints.plotSmoothPtermFactorgg <- function(a){
  
  if( is.null(a$shape) ){ a$shape <- 19}
  if( is.null(a$size) ){ a$size <- 2}
  
  # Substituting residuals data with fit data, so we can use code for residuals points plot
  a$data$res <- a$data$fit
  a$data$res$y <- a$data$res$ty
  a$data$res$sub <- rep(TRUE, nrow(a$data$res))
  
  l_points.plotSmoothPtermFactorgg( a )
  
}