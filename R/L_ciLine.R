#'
#' Adding confidence intervals to effect plot
#' 
#' @description This layer adds confidence interval lines to smooth, random or parametric effect plots.
#'
#' @param level coverage level (e.g. 0.9 means 90\% intervals). Should be in (0, 1).
#' @param mul number multiplied by the standard errors when calculating 
#'            standard error curves. By default \code{NULL}, if
#'            set to a positive number it will over-ride \code{level}.
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_line}.
#' @return An object of class \code{gamLayer}.
#' @seealso See [plot.mgcv.smooth.1D], [plot.ptermNumeric] or [plot.random.effect]  for examples.
#' @export l_ciLine
#'
l_ciLine <- function(level = 0.95, mul = NULL, ...){
  
  arg <- list(...)
  arg$xtra <- list("level" = level, "mul" = mul)
  o <- structure(list("fun" = "l_ciLine",
                      "arg" = arg), 
                 class = "gamLayer")
  
  return(o)
  
}

######## Internal method 
#' @noRd
l_ciLine.1D <- l_ciLine.PtermNumeric <- function(a){
  
  xtra <- a$xtra
  a$xtra <- NULL
  
  if( is.null(xtra$mul) ) { xtra$mul <- qnorm((xtra$level+1)/2)  }
  
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

######## Internal method 
#' @noRd
l_ciLine.randomEffect <- function(a){
  
  xtra <- a$xtra
  a$xtra <- NULL
  
  # Over-ride level
  if( !is.null(xtra$mul) ) { xtra$level <- 2 * pnorm(xtra$mul) - 1  }
  
  # Add CI lines to data: the confidence intervals here are based on the
  # formula in "Worm plot: a simple diagnostic device for modelling growth reference curves"
  # page 6. We need to multiply them by sd(.dat$y) because we are not normalizing the
  # random effects. Simulation results indicate that multiplying is the right thing to do.
  .dat <- a$data$fit[ c("x", "y") ]
  .trans <- a$data$misc$trans
  
  .n <- nrow( .dat )
  .p <- ppoints( .n )
  .alp <- (1 - xtra$level)/2
  .con <- sd(.dat$y) * qnorm(.alp) * sqrt(.p * (1 - .p)/.n) / dnorm(.dat$x)
  .lin <- as.numeric( qqline(y = .dat$y)$data ) # Use qqline to get intercept and slope
  
  .dat$uci <- .trans( .lin[1] + .lin[2] * .dat$x + .con )
  .dat$lci <- .trans( .lin[1] + .lin[2] * .dat$x - .con )
  a$data <- .dat
  
  if( is.null(a$linetype) ){ a$linetype <- 3}
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

######## Internal method 
# Adding CI to plot where different QQ-plots appear (corresponding to  random effect estimated on
# different data) does not make sense as the CI depend on the variance of the estimated
# random effect, which is different for each run.
# #' @noRd
# l_ciLine.MultiRandomEffect <-  function(a){
#   
#   # Need use only data from one of the quantile, otherwise l_ciLine.randomEffect thinks
#   # that we have n * number_of_quantiles responses, rather than just n
#   a$data$fit <- a$data$fit[a$data$fit$id == levels(a$data$fit$id)[1], ]
#   
#   return( l_ciLine.randomEffect(a) )
#   
# }
