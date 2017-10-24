#'
#' Add rug to smooth effect plot
#' 
#' @description XXX
#'
#' @param jit should the x's and y's be jittered.
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_rug}.
#' @return an object of class \code{gamLayer}.
#' @export l_rug
#'
l_rug <- function(jit = c(FALSE, FALSE), ...){
  arg <- list(...)
  arg$xtra <- list("jit" = jit)
  o <- structure(list("fun" = "l_rug",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method for 1D plots
#' @noRd
l_rug.plotSmooth1Dgg <- l_rug.plotSmoothCheck1Dgg <- function(a){
  
  if( is.null(a$mapping) ) { a$mapping <- aes(x = x) }
  
  .l_rug.plotSmoothgg( a )
  
}

######## Internal method for 2D plots
#' @noRd
l_rug.plotSmooth2Dgg <- l_rug.plotSmoothsos0gg <- 
                        l_rug.plotSmoothsos1gg <- l_rug.plotSmoothCheck2Dgg <- function(a){
  
  if( is.null(a$mapping) ) { a$mapping <- aes(x = x, y = y) }
  
  .l_rug.plotSmoothgg( a )
  
}

######## Internal method
#' @noRd
.l_rug.plotSmoothgg <- function(a){
  a$data <- a$data$res[a$data$res$sub, ]
  a$inherit.aes <- FALSE
  if( is.null(a$size) ){ a$size <- 0.2 }
  if( is.null(a$data$y)){ a$mapping <- aes(x = x) }
  
  # Jitter if necessary
  jit <- a$xtra$jit
  if( length(jit) == 1 ){ jit <- c(jit, jit) }
  if(a$xtra$jit[1]){ a$data$x <- jitter(a$data$x) }
  if(a$xtra$jit[2] || !is.null(a$data$y)){ a$data$y <- jitter(a$data$y) }
  a$xtra <- NULL
  
  fun <- "geom_rug"
  out <- do.call(fun, a)
  return( out )
}
