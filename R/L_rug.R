#'
#' Add rug to smooth effect plot
#' 
#' @description XXX
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_rug}.
#' @return an object of class \code{gamLayer}.
#' @export l_rug
#'
l_rug <- function(...){
  arg <- list(...)
  o <- structure(list("fun" = "l_rug",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method for factor 1D plots
#' @noRd
l_rug.plotSmoothPtermFactorgg <- l_rug.plotSmoothCheck1DFactorgg <- function(a){
  
  if( is.null(a$position) ) { a$position <- position_jitter(width = 0.25, height = 0) }
  
  if( is.null(a$data$res$y) ){ a$mapping <- aes(x = x) }
  
  l_rug.plotSmooth1Dgg( a )
  
}

######## Internal method for logical 1D plots
l_rug.plotSmoothCheck1DLogicalgg <- function(a){
  
  if( is.null(a$position) ) { a$position <- position_jitter(width = 0.45, height = 0) }
  
  if( is.null(a$data$res$y) ){ a$mapping <- aes(x = x) }
  
  l_rug.plotSmooth1Dgg( a )
  
}

######## Internal method for numeric 1D plots
#' @noRd
l_rug.plotSmooth1Dgg <- l_rug.plotSmoothCheck1DNumericgg <- 
l_rug.plotSmoothPtermNumericgg <- function(a){
  
  if( is.null(a$mapping) ) { a$mapping <- aes(x = x) }
  
  .l_rug.plotSmoothgg( a )
  
}

######## Internal method for factor/numeric 2D plots
#' @noRd
l_rug.plotSmoothCheck2DFactorNumericgg <- function(a){
  
  if( is.null(a$mapping) ) { a$mapping <- aes(x = x) }
  
  if( is.null(a$position) ) { a$position <- position_jitter(width = 0.25, height = 0) }
  
  l_rug.plotSmoothCheck2DNumericNumericgg( a )
  
}

######## Internal method for factor/factor 2D plots
#' @noRd
l_rug.plotSmoothCheck2DFactorFactorgg <- function(a){
  
  if( is.null(a$mapping) ) { a$mapping <- aes(x = x) }
  
  if( is.null(a$position) ) { a$position <- position_jitter(width = 0.25, height = 0.25) }
  
  l_rug.plotSmoothCheck2DNumericNumericgg( a )
  
}

######## Internal method for numeric/numeric 2D plots
#' @noRd
l_rug.plotSmooth2Dgg <- l_rug.plotSmoothsos0gg <- l_rug.plotSmoothsos1gg <- 
l_rug.plotSmoothCheck2DNumericNumericgg <- function(a){
  
  if( is.null(a$mapping) ) { a$mapping <- aes(x = x, y = y) }
  
  .l_rug.plotSmoothgg( a )
  
}

######## General internal method
#' @noRd
.l_rug.plotSmoothgg <- function(a){
  a$data <- a$data$res[a$data$res$sub, ]
  a$inherit.aes <- FALSE
  if( is.null(a$size) ){ a$size <- 0.2 }
  
  if( is.null(a$mapping$y) ){
    a$mapping <-  aes(x = x, y = y)
    a$sides <- "b"
  }
  if( is.null(a$mapping$x) ){
    a$mapping <-  aes(x = x, y = y)
    a$sides <- "l"
  }
  if( is.null(a$data$y) ){
    a$data$y <- NaN
  }

  fun <- "geom_rug"
  out <- do.call(fun, a)
  return( out )
}
