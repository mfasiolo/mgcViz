#'
#' Adding confidence intervals to barplots
#' 
#' @description Adding confidence intervals to barplot.
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_errorbar}.
#' @return an object of class \code{gamLayer}.
#' @export l_ciBar
#'
l_ciBar <- function(level = 0.95, mul = NULL, ...){
  
  arg <- list(...)
  arg$xtra <- list("level" = level, "mul" = mul)
  o <- structure(list("fun" = "l_ciBar",
                      "arg" = arg), 
                 class = "gamLayer")
  
  return(o)
  
}

######## Internal method for factor plots
#' @noRd
#'
l_ciBar.plotSmoothPtermFactorgg <- function(a){
  
  xtra <- a$xtra
  a$xtra <- NULL
  
  if( is.null(xtra$mul) ) { xtra$mul <- qnorm((xtra$level+1)/2)  }
  
  # Add CI lines to data
  .dat <- a$data$fit
  .trans <- a$data$misc$trans
  .dat$uci <- .trans( .dat$y + xtra$mul * .dat$se )
  .dat$lci <- .trans( .dat$y - xtra$mul * .dat$se )
  a$data <- .dat
  
  if( is.null(a$width) ){ a$width <- 0.5 }
  if( is.null(a$linetype) ){ a$linetype <- 2 }
  
  a$mapping <- aes(ymin = lci, ymax = uci)
  out <- do.call("geom_errorbar", a)
  return( out )
  
}
