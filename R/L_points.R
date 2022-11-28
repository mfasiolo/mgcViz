#'
#' Add points to plot
#' 
#' @description This layers add points to smooth, parametric or random effect plots.
#'              It can also be used to add points to the output of \code{check1D}
#'              and \code{check2D}. The meaning of the added points, which could represent
#'              residuals or covariate values, should be clear from context.
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_point}.
#' @return An object of class \code{gamLayer}.
#' @seealso See [plot.mgcv.smooth.1D], [plot.mgcv.smooth.2D], [check1D] or [check2D] for examples.
#' @export l_points
#'
l_points <- function(...){
  arg <- list(...)
  o <- structure(list("fun" = "l_points",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}


######## Internal method for factor 1D plots
#' @noRd
l_points.PtermFactor <- l_points.Check1DFactor <- function(a){
  
  if( is.null(a$position) ) { a$position <- position_jitter(width = 0.25, height = 0) }
  
  l_points.1D( a )
  
}

######## Internal method for logical 1D plots
#' @noRd
l_points.Check1DLogical <- function(a){
    
    if( is.null(a$position) ) { a$position <- position_jitter(width = 0.45, height = 0) }
    
    l_points.1D( a )
    
}

######## Internal method for factor/numeric 2D checks
#' @noRd
l_points.Check2DFactorNumeric <- function(a){
  
  if( is.null(a$position) ) { a$position <- position_jitter(width = 0.25, height = 0) }
  
  l_points.1D(a)
  
}

######## Internal method for factor/factor 2D checks
#' @noRd
l_points.Check2DFactorFactor <- function(a){
  
  if( is.null(a$position) ) { a$position <- position_jitter(width = 0.25, height = 0.25) }
  
  l_points.1D(a)
  
}

######## Internal method for numeric/numeric 2D plots
#' @noRd
l_points.2D <- l_points.Check2DNumericNumeric <- l_points.MDslice <- function(a){
  
  return( l_points.1D(a) )
  
}

######## General internal method 
#' @noRd
l_points.1D <- l_points.sos1 <- l_points.sos0 <- 
l_points.Check1DNumeric <- l_points.PtermNumeric <- function(a){
  
  a$data <- a$data$res[a$data$res$sub, ]
  
  if( is.matrix(a$data$y) ){
    tmp <- as.vector(a$data$y)
    a$data <- data.frame(x = rep(a$data$x, length(tmp)/nrow(a$data)), 
                         y = tmp)
  }
  
  a$inherit.aes <- FALSE
  if( is.null(a$mapping)){ a$mapping <- aes(x = x, y = y) }
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

######## Internal method for random effects
#' @noRd
l_points.randomEffect <- l_points.MultiRandomEffect <- function(a){
    
    a$data <- a$data$fit
    if( is.null(a$shape) ) { a$shape <- 19 } 

    fun <- "geom_point"
    out <- do.call(fun, a)

    return( out )
  
}



