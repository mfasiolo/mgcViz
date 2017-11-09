#'
#' Add residuals to smooth effect plot
#' 
#' @description XXX
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_point}.
#' @return an object of class \code{gamLayer}.
#' @export l_points
#'
l_points <- function(jit = c(FALSE, FALSE), ...){
  arg <- list(...)
  arg$xtra <- list("jit" = jit)
  o <- structure(list("fun" = "l_points",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}


######## Internal method for factor plots
#' @noRd
l_points.plotSmoothPtermFactorgg <- l_points.plotSmoothCheck1DFactorgg <- 
l_points.plotSmoothCheck1DLogicalgg <- function(a){
  
  a$data$res$x <- as.numeric( a$data$res$x )
  
  l_points.plotSmooth1Dgg( a )
  
}

######## Internal method for numeric parametric (non-smooth) plots
#' @noRd
l_points.plotSmoothPtermNumericgg  <- function(a){
  
  if( is.null(a$data$res$y) ){ 
    message("l_points(): Partial residuals are not available")  
    return( NULL )
  }
 
  l_points.plotSmooth1Dgg( a )
   
}

######## Internal method 
#' @noRd
l_points.plotSmooth1Dgg <- l_points.plotSmoothsos1gg <- l_points.plotSmoothsos0gg <- 
l_points.plotSmoothCheck1DNumericgg <- function(a){
  
  a$data <- a$data$res[a$data$res$sub, ]
  
  # Jitter if necessary
  jit <- a$xtra$jit
  if( length(jit) == 1 ){ jit <- c(jit, jit) }
  if(jit[1]){ a$data$x <- jitter(a$data$x) }
  if(jit[2] && !is.null(a$data$y)){ a$data$y <- jitter(a$data$y) }
  a$xtra <- NULL
  
  a$mapping <- aes(x = x, y = y)
  a$inherit.aes <- FALSE
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

######## Internal method 
#' @noRd
l_points.plotSmoothrandomEffectgg <- function(a){
    
    a$data <- a$data$fit
    if( is.null(a$shape) ) { a$shape <- 19 } 

    fun <- "geom_point"
    out <- do.call(fun, a)

    return( out )
  
}

######## Internal method 
#' @noRd
l_points.plotSmooth2Dgg <- l_points.plotSmoothCheck2Dgg <- function(a){
  
  return( l_points.plotSmooth1Dgg(a) )
  
}

