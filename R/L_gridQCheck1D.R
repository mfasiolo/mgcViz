#'
#' Checking sign of residuals along one covariate
#' 
#' @description This layer is mainly useful when checking quantile GAMs fitted using the \code{qgam}
#'              package. The residuals, r, are binned according to the corresponding value of a 
#'              covariate, x. Then the proportions of negative residuals within each bin are calculated, and
#'              compared with the theoretical value, \code{qu}. Confidence intervals for the proportion
#'              of negative residuals can be derived using binomial quantiles (under an independence
#'              assumption). To be used in conjuction with [check1D].
#' @name l_gridQCheck1D
#' @param qu the quantile of interest. Should be in (0, 1).
#' @param n number of grid intervals.
#' @param level the level of the confidence intervals plotted.
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_point}.
#' @return An object of class \code{gamLayer}
#' @examples 
#' # Simulate some data
#' library(mgcViz)
#' set.seed(3841)
#' dat <- gamSim(1,n=400,dist="normal",scale=2)
#' dat$fac <- as.factor( sample(letters[1:8], nrow(dat), replace = TRUE) ) 
#' fit <- qgam(y~s(x1)+s(x2)+s(x3)+fac, data=dat, err = 0.05, qu = 0.4)
#' fit <- getViz(fit)
#' 
#' # "x0" effect is missing, but should be there. l_gridQCheck1D shows
#' # that fraction of negative residuals is quite different from the theoretical 0.4
#' # in several places along "x0".
#' check1D(fit, dat$x0) + l_gridQCheck1D(qu = 0.4, n = 20)
#' # The problem gets better if s(x0) is added to the model.
#' 
#' # Works also with factor variables
#' check1D(fit, "fac") + l_gridQCheck1D(qu = 0.4)
#' @importFrom matrixStats colSds
#' @importFrom plyr aaply
#' @importFrom stats qbinom
#' @rdname l_gridQCheck1D
#' @export l_gridQCheck1D
l_gridQCheck1D <- function(qu, n = 20, level = 0.8, ...){
  arg <- list(...)
  arg$xtra <- list("qu" = qu, "n" = n, "level" = level, "stand" = "none")
  o <- structure(list("fun" = "l_gridQCheck1D",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method for numeric covariates
#' @noRd
l_gridQCheck1D.Check1DNumeric <- function(a){
  
  a$xtra$class <- "numeric"
  
  .l_gridQCheck1D( a )
  
}

######## Internal method for factor covariates 
#' @noRd
l_gridQCheck1D.Check1DFactor <- l_gridQCheck1D.Check1DLogical <- function(a){
  
  a$xtra$class <- "factor"
  a$data$res$x <- as.factor( a$data$res$x )
  
  .l_gridQCheck1D( a )
  
}

######## Internal method 
#' @noRd
.l_gridQCheck1D <- function(a){
  
  funCreator <- function(.qu, .lev, .type){
    .ciFun <- function(.x){
      .n <- length(.x)
      if( .type == "lower" ){ return( qbinom((1-.lev)/2, .n, .qu) / .n ) }
      if( .type == "upper" ){ return( qbinom((1-.lev)/2, .n, .qu, lower.tail = FALSE) / .n ) }
    }
    return( .ciFun )
  }
  
  xtra <- a$xtra
  a$xtra$level <- 0
  a$data$sim <- NULL
  
  out <- list()
  a$xtra$gridFun <- function(.x){ sum(.x <= 0) / length(.x) }
  out[[1]] <- .l_gridCheck1D(a)[[1]]
  
  a$colour <- 2
  a$shape <- 3
  a$size <- 2
  a$xtra$gridFun <- funCreator(.qu = xtra$qu, .lev = xtra$level, .type = "lower")
  out[[2]] <- .l_gridCheck1D(a)[[1]]
  
  a$xtra$gridFun <- funCreator(.qu = xtra$qu, .lev = xtra$level, .type = "upper")
  out[[3]] <- .l_gridCheck1D(a)[[1]]
  
  out[[4]] <-  geom_hline(yintercept = xtra$qu, linetype = 2)
  
  out[[5]] <-  ylab("Proportion of neg. resid.")
  
  class(out) <- "listOfLayers"
  
  return( out )
  
}

