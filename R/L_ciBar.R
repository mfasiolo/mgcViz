#'
#' Adding confidence intervals to barplots
#' 
#' @description This layer adds confidence intervals to barplots, such as those produced by factor effects GAM.
#'
#' @param level the level of the confidence intervals (e.g. 0.9 means 90\% intervals).
#' @param mul number multiplied by the standard errors when calculating 
#'            standard error curves. By default \code{NULL}, if
#'            set to a positive number it will over-ride \code{level}.
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_errorbar}.
#' @return An object of class \code{gamLayer}.
#' @seealso See [plot.ptermFactor] for examples.
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
l_ciBar.PtermFactor <- l_ciBar.MultiPtermNumeric <- l_ciBar.MultiPtermFactor <- function(a){
  
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
