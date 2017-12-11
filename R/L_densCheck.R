#'
#' Checking residuals conditional density
#' 
#' @description This layer calculates and plots how the empirical conditional density of
#'              the residuals, r, differs from its theoretical or model-based counterpart, 
#'              along a covariate, x.
#'              
#' @param n vector of two positive integers, indicating the number of grid points
#'          at which the density is evaluated on the x and r axes.
#' @param bw vector with two positive entries, indicating the bandwidth to be used
#'           by the kernel density estimator of \code{p(r|x)} along x and r.
#' @param tol small positive numerical tolerance. The estimated density at a certain 
#'            location is set to \code{NA} (hence it will appear white) when it falls 
#'            below \code{tol/sqrt(2*pi*sig)}, where \code{sig} is the standard 
#'            deviation of the residuals. Set \code{tol} to -1 plot the density on 
#'            the whole x-y plane, no matter how low it is.
#' @param dFun function used to compute the difference between the empirical (em) and theoretical (th)
#'             conditional density of the residuals. By default it is \code{abs(sqrt(em)-sqrt(th))^(1/3)}, 
#'             where \code{th} is computed using either a uniform or a normal density, depending on the
#'             type of residuals used in the [check1D] call. It should as arguments three vectors: \code{.ed} (the empirical
#'             conditional density), \code{.gr} (the points along y where the density is evaluated) and
#'             \code{.y} (the partial residuals).
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_raster}.
#' @return An object of class \code{gamLayer}.
#' @details This layer is mainly meant to work together with the [check1D] function. The default \code{dFun}
#'          if compare the conditional distribution of the residuals with N(0, 1) or Unif(0, 1) densities, 
#'          depending on the residual type used in the [check1D] call.
#' @examples 
#' library(mgcViz);
#' 
#' # Dataset where variance increases linearly with x2, for x2 > 0.2
#' n <- 1e3
#' x1 <- rnorm(1e3)
#' x2 <- rnorm(1e3)
#' dat <- data.frame("x1"=x1, 
#'                   "x2"=x2, "y"=sin(x1) + 0.5*x2^2 + pmax(x2, 0.2)*rnorm(n))
#' b <- gam(y ~ s(x1)+s(x2), data=dat)
#' b <- getViz(b)
#' 
#' # (Blue) Yellow indicates area where the empirical density 
#' # of the residuals is (lower) higher than it should be under 
#' # the model (residuals should be N(0, sigma) here).
#' # Here there are clear signs of heteroscedasticity: 
#' # the conditional variance is is increasing for x2 > 0.2. 
#' check1D(b, "x2") + l_densCheck() + l_rug()
#'
#' @rdname l_densCheck
#' @export l_densCheck
#'
l_densCheck <- function(n=c(80, 80), bw=NULL, tol=1e-6, dFun=NULL, ...){
  arg <- list(...)
  arg$xtra <- list("n"=n, "bw"=bw, "tol"=tol, "dFun"=dFun)
  o <- structure(list("fun" = "l_densCheck",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method 
#' @noRd    
l_densCheck.Check1DNumeric <- function(a){
  
  ### 1. Preparation
  xtra <- a$xtra
  a$xtra <- NULL
  
  if( is.null(xtra$dFun) ){
    xtra$dFun <- function(.ed, .gr, .y){
      if( a$data$misc$resType == "tunif" ){ # Comparing with uniform density
        d <- sqrt(.ed) - 1
      } else { # Comparing with Gaussian
        d <- dnorm(.gr, 0, sd=sd(.y))
        d <- sqrt(.ed) - sqrt(d)
      }
      return( sign(d) * abs(d) ^ (1/3) )
    }
  }
  
  ### 2. Density Estimation 
  # Computed joint or conditional residual density
  estYcX <- .fastKernDens(dat = a$data$res, xlimit = NULL, ylimit = NULL,
                          cond = TRUE, bw = xtra$bw, ngr = xtra$n, tol = xtra$tol)$dXY
  
  ### 3. Plotting
  a$data <- data.frame("z" = xtra$dFun(.ed=as.numeric(t(estYcX$fhat)), 
                                       .gr=estYcX$x2, .y=a$data$res$y), 
                       "x" = rep(estYcX$x1, each=xtra$n[1]), 
                       "y" = rep(estYcX$x2, xtra$n[2]))
  a$mapping <- aes(x = x, y = y, fill = z)
  a$inherit.aes <- FALSE
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE }
  
  # Build layers
  out <- list()
  out[[1]] <- do.call("geom_raster", a) 
  out[[2]] <- scale_fill_gradientn(colours = viridis(50, begin = 0.2), na.value = "white") 

  class(out) <- "listOfLayers"
  
  return( out )
  
}

