#'
#' Adding rug to margins of a plot
#' 
#' @description This layer adds a rug plot to the margins of a plot. It is mainly a 
#'              wrapper around [ggplot2::geom_rug]. Notice that for factor effects
#'              plots the rug is jittered by default.
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_rug}.
#' @return An object of class \code{gamLayer}.
#' @seealso See [plot.mgcv.smooth.1D], [plot.mgcv.smooth.2D] or [check1D] for examples.
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
l_rug.PtermFactor <- l_rug.Check1DFactor <- function(a){
  
  if( is.null(a$position) ) { a$position <- position_jitter(width = 0.25, height = 0) }
  
  if( is.null(a$data$res$y) ){ a$mapping <- aes(x = x) }
  
  l_rug.1D( a )
  
}

######## Internal method for logical 1D plots
l_rug.Check1DLogical <- function(a){
  
  if( is.null(a$position) ) { a$position <- position_jitter(width = 0.45, height = 0) }
  
  if( is.null(a$data$res$y) ){ a$mapping <- aes(x = x) }
  
  l_rug.1D( a )
  
}

######## Internal method for numeric 1D plots
#' @noRd
l_rug.1D <- l_rug.Multi1D <- l_rug.Check1DNumeric <- l_rug.PtermNumeric <- function(a){
  
  if( is.null(a$mapping) ) { a$mapping <- aes(x = x) }
  
  .l_rug( a )
  
}

######## Internal method for factor/numeric 2D plots
#' @noRd
l_rug.Check2DFactorNumeric <- function(a){
  
  if( is.null(a$mapping) ) { a$mapping <- aes(x = x) }
  
  if( is.null(a$position) ) { a$position <- position_jitter(width = 0.25, height = 0) }
  
  l_rug.Check2DNumericNumeric( a )
  
}

######## Internal method for factor/factor 2D plots
#' @noRd
l_rug.Check2DFactorFactor <- function(a){
  
  if( is.null(a$mapping) ) { a$mapping <- aes(x = x) }
  
  if( is.null(a$position) ) { a$position <- position_jitter(width = 0.25, height = 0.25) }
  
  l_rug.Check2DNumericNumeric( a )
  
}

######## Internal method for numeric/numeric 2D plots
#' @noRd
l_rug.2D <- l_rug.sos0 <- l_rug.sos1 <- l_rug.Check2DNumericNumeric <- l_rug.MDslice <- function(a){
  
  if( is.null(a$mapping) ) { a$mapping <- aes(x = x, y = y) }
  
  .l_rug( a )
  
}

######## General internal method
#' @noRd
.l_rug <- function(a){
  a$data <- a$data$res[a$data$res$sub, ]
  a$inherit.aes <- FALSE
  if( is.null(a$size) ){ a$size <- 0.2 }
  
  if( is.null(a$mapping$y) && ("PositionJitter" %in% class(a$position)) ){
    a$mapping <-  aes(x = x, y = y)
    a$sides <- "b"
  }
  if( is.null(a$mapping$x) && ("PositionJitter" %in% class(a$position)) ){
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
