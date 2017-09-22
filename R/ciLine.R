#'
#' Add confidence curves to plot
#' 
#' @description XXX
#'
#' @param mul number multiplied by the standard errors when calculating 
#'            standard error curves or surfaces.
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_line}.
#' @return An object of class \code{gamLayer}.
#' @export ciLine
#'
ciLine <- function(mul = 2, ...){
  arg <- list(...)
  arg$xtra <- list("mul" = mul)
  o <- structure(list("fun" = "ciLine",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method 
#' @noRd
ciLine.plotSmooth1D <- function(a){
  
  xtra <- a$xtra
  a$xtra <- NULL
  
  # Add CI lines to data
  .dat <- a$data$fit[ c("x", "y", "se") ]
  .trans <- a$data$misc$trans
  .dat$uci <- .trans( .dat$y + xtra$mul * .dat$se )
  .dat$lci <- .trans( .dat$y - xtra$mul * .dat$se )
  a$data <- .dat
  
  if( is.null(a$linetype) ){ a$linetype <- "dashed"}
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE}
  a$inherit.aes <- FALSE
  
  # Call ggplot2::geom_line
  out <- list()
  a$mapping  <- aes(x = x, y = uci)
  out[[1]] <- do.call("geom_line", a)
  
  a$mapping  <- aes(x = x, y = lci)
  out[[2]] <- do.call("geom_line", a)
  class(out) <- "listOfLayers"
  
  return( out )
}
