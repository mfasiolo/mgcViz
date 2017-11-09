#'
#' Add residuals to smooth effect plot
#' 
#' @description XXX
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_point}.
#' @return an object of class \code{gamLayer}.
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
l_points.plotSmoothPtermFactorgg <- l_points.plotSmoothCheck1DFactorgg <- function(a){
  
  if( is.null(a$position) ) { a$position <- position_jitter(width = 0.25, height = 0) }
  
  l_points.plotSmooth1Dgg( a )
  
}

######## Internal method for logical 1D plots
#' @noRd
l_points.plotSmoothCheck1DLogicalgg <- function(a){
    
    if( is.null(a$position) ) { a$position <- position_jitter(width = 0.45, height = 0) }
    
    l_points.plotSmooth1Dgg( a )
    
}

######## Internal method for factor/numeric 2D checks
#' @noRd
l_points.plotSmoothCheck2DFactorNumericgg <- function(a){
  
  if( is.null(a$position) ) { a$position <- position_jitter(width = 0.25, height = 0) }
  
  l_points.plotSmooth1Dgg(a)
  
}

######## Internal method for factor/factor 2D checks
#' @noRd
l_points.plotSmoothCheck2DFactorFactorgg <- function(a){
  
  if( is.null(a$position) ) { a$position <- position_jitter(width = 0.25, height = 0.25) }
  
  l_points.plotSmooth1Dgg(a)
  
}

######## Internal method for numeric/numeric 2D plots
#' @noRd
l_points.plotSmooth2Dgg <- l_points.plotSmoothCheck2DNumericNumericgg <- function(a){
  
  return( l_points.plotSmooth1Dgg(a) )
  
}

######## General internal method 
#' @noRd
l_points.plotSmooth1Dgg <- l_points.plotSmoothsos1gg <- l_points.plotSmoothsos0gg <- 
l_points.plotSmoothCheck1DNumericgg <- l_points.plotSmoothPtermNumericgg <- function(a){
  
  a$data <- a$data$res[a$data$res$sub, ]
  
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
l_points.plotSmoothrandomEffectgg <- function(a){
    
    a$data <- a$data$fit
    if( is.null(a$shape) ) { a$shape <- 19 } 

    fun <- "geom_point"
    out <- do.call(fun, a)

    return( out )
  
}



