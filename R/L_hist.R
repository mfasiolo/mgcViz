#'
#' Adding histogram to a plot
#' 
#' @description This layer adds a histogram to a plot. It is mainly a 
#'              wrapper around [ggplot2::geom_histogram]. 
#'
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_histogram}.
#' @return An object of class \code{gamLayer}.
#' @seealso See [check0D] for examples.
#' @importFrom stats density
#' @export l_hist
#'
l_hist <- function(...){
  arg <- list(...)
  o <- structure(list("fun" = "l_hist",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}


######## Internal method for posterior checks: vector case
#' @noRd
l_hist.Check0DVectorNumeric <- function(a){
  
  # Get data out of original object
  obs <- a$data$res$x
  sim <- a$data$sim
  nsim <- ifelse(is.null(sim), 0, nrow(sim))
  n <- length(obs)
  
  a$data <- data.frame("x" = c(obs, as.vector(sim)), 
                       "id" = factor(c(rep("obs", n), rep("sim", n*nsim))))
  
  if( is.null(a$mapping) ) { 
    a$mapping <- aes("x" = x, "y" = stat(density), "colour" = id, "fill" = id)
  }
  
  a$inherit.aes <- FALSE
  if( is.null(a$alpha) ){ a$alpha <- 0.5 }
  if( is.null(a$position) ){ a$position <- "identity" }
  
  fun <- "geom_histogram"
  out <- do.call(fun, a)
  
  return( out )
  
}


######## Internal method for posterior checks: scalar case
#' @noRd
l_hist.Check0DScalarNumeric <- function(a){
  
  if( is.null(a$data$sim) ){
    message("l_hist: gamViz object does not contain any simulation, so there is nothing to plot")
    return(NULL)
  }
  
  a$data <- data.frame("x" = as.vector(a$data$sim))
  
  if( is.null(a$mapping) ) { 
    a$mapping <- aes("x" = x, "y" = stat(density))
  }
  if( is.null(a$fill) ){ a$fill = "#56B4E9" }
  
  a$inherit.aes <- FALSE
  
  fun <- "geom_histogram"
  out <- do.call(fun, a)
  
  return( out )
  
}