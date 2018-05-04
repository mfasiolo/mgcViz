#'
#' Adding confidence band to effect plots
#' 
#' @description This layer adds a polygon representing the confidence band of a 
#'              smooth, random or parametric effect plots.
#'
#' @param level coverage level (e.g. 0.9 means 90\% intervals). Should be in (0, 1).
#' @param mul number multiplied by the standard errors when calculating 
#'            standard error curves. By default \code{NULL}, if
#'            set to a positive number it will over-ride \code{level}.
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_polygon}.
#' @return An object of class \code{gamLayer}
#' @seealso See [plot.mgcv.smooth.1D], [plot.ptermNumeric] or [plot.random.effect] for examples.
#' @export l_ciPoly
#'
l_ciPoly <- function(level = 0.95, mul = NULL, ...){
  
  arg <- list(...)
  arg$xtra <- list("level" = level, "mul" = mul)
  o <- structure(list("fun" = "l_ciPoly",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
  
}

######## Internal method 
#' @noRd
l_ciPoly.1D <- l_ciPoly.PtermNumeric <- function(a){
  
  xtra <- a$xtra
  a$xtra <- NULL
  
  if( is.null(xtra$mul) ) { xtra$mul <- qnorm((xtra$level+1)/2)  }
  
  # Create dataframe for polygon
  .dat <- a$data$fit[ c("x", "y", "se") ]
  .trans <- a$data$misc$trans
  a$data <- data.frame("x" = c(.dat$x, rev(.dat$x)), 
                       "y" = c(.trans( .dat$y + xtra$mul * .dat$se ), 
                               rev(.trans( .dat$y - xtra$mul * .dat$se ))))
  
  a$mapping  <- aes(x = x, y = y)
  a$inherit.aes <- FALSE
  if( is.null(a$fill) ){ a$fill <- "light grey"}
  
  out <- do.call("geom_polygon", a)
  
  return( out )
}


######## Internal method 
#' @noRd
l_ciPoly.randomEffect <- function(a){
  
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
  
  # Create dataframe for polygon
  a$data <- data.frame("x" = c(.dat$x, rev(.dat$x)), 
                       "y" = c(.trans( .lin[1] + .lin[2] * .dat$x + .con ), 
                               rev(.trans( .lin[1] + .lin[2] * .dat$x - .con ))))
  
  a$mapping  <- aes(x = x, y = y)
  a$inherit.aes <- FALSE
  if( is.null(a$fill) ){ a$fill <- "light grey"}
  
  out <- do.call("geom_polygon", a)
  
  return( out )
}

######## Internal method 
#' @noRd
l_ciPoly.MultiRandomEffect <-  function(a){
  
  # Need use only data from one of the quantile, otherwise l_ciPoly.randomEffect thinks
  # that we have n * number_of_quantiles responses, rather than just n
  a$data$fit <- a$data$fit[a$data$fit$qu == levels(a$data$fit$qu)[1], ]
  
  return( l_ciPoly.randomEffect(a) )
  
}