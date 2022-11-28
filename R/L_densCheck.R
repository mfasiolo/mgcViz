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
#'             conditional density of the residuals. By default it is \code{(sqrt(em)-sqrt(th))^(1/3)}, 
#'             where \code{th} is computed using either a uniform or a normal density, depending on the
#'             type of residuals used in the [check1D] call. It should have as arguments three vectors: \code{.ed} (the empirical
#'             conditional density), \code{.gr} (the points along the y-axis where the density is evaluated) and
#'             \code{.y} (the residuals).
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_raster}.
#' @return An object of class \code{gamLayer}.
#' @details This layer is mainly meant to work together with the [check1D] function. If check1D() is called with
#'          residual type == "tunif" or "tnormal", then \code{l_densCheck} compares the conditional distribution 
#'          of the residuals with Unif(0, 1) or N(0, 1). By changing the distance function \code{dFun}
#'          one could of course change both the distance metric and the reference distribution (see Examples below). \cr \cr
#'          WARNING: if check1D() is called with type != "tunif" or "tnormal", then the default distance used by l_densCheck is \cr
#'          \code{dFun <- function(.ed, .gr, .y)} \code{\{}\cr
#'          \code{d <- dnorm(.gr, 0, sd=sd(.y)) # sd=sd(.y) !!!}\cr
#'          \code{d <- sqrt(.ed)-sqrt(d) }\cr
#'          \code{return(sign(d)*abs(d)^(1/3))}\cr
#'          \code{\}} \cr
#'          so the residuals are standardized using their own std dev \code{sd(.y)}.
#'          Hence \code{l_densCheck} might not detect that the mean estimated variance 
#'          under the fitted model is different from the residuals variance. 
#'          Hence it is safer to use residual types "tunif" or "tnormal", or a 
#'          customized distance function dFun (see below for an example on how to do this).
#' @examples 
#' library(mgcViz);
#' # Dataset where variance increases linearly with x2, for x2 > 0.2
#' n <- 1e3
#' x1 <- rnorm(1e3)
#' x2 <- rnorm(1e3)
#' dat <- data.frame("x1"=x1, 
#'                   "x2"=x2, "y"=sin(x1) + 0.5*x2^2 + pmax(x2, 0.2)*rnorm(n))
#' b <- gam(y ~ s(x1)+s(x2), data=dat)
#' b <- getViz(b)
#' 
#' # (Red) Blue indicates area where the empirical density 
#' # of the residuals is (lower) higher than it should be under 
#' # the model (residuals should be N(0, sigma) here).
#' # Here there are clear signs of heteroscedasticity: 
#' # the conditional variance is is increasing for x2 > 0.2. 
#' check1D(b, "x2", type = "tnormal") + l_densCheck() + l_rug()
#' 
#' # Suppose we want to compare the conditional density of the standardized residuals
#' # not with a Gaussian, but with a Student-t density with 3 degree of freedom.
#' # We could achieve this as follows:
#' myDistance <- function(.ed, .gr, .y){
#'   d <- dt(.gr / sd(.y), df = 3)
#'   d <- abs( sqrt(.ed) - sqrt(d) ) # We are using absolute difference between sqrt-densities 
#' }
#' 
#' check1D(b, "x2", type = "response") + l_densCheck(dFun = myDistance) + l_rug()
#' # NB comparing with a Student density is not useful for this example, but it illustrates
#' # how both the distance function and the reference density can be customized.
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
  
  if(a$data$misc$resType == "y"){ message("Using l_densCheck might not make sense with residual type == \"y\". See ?check1D")}
  
  ### 1. Preparation
  xtra <- a$xtra
  a$xtra <- NULL
  
  if( is.null(xtra$dFun) ){
    if( !(a$data$misc$resType %in% c("tunif", "tnormal")) ){ 
      message("It is safer to use l_densCheck when residual type is set to \"tnormal\" or \"tunif\" in check1D. \n Otherwise it is possible to supply a customized residuals function dFun(). See ?l_densCheck") 
    }
    
    xtra$dFun <- function(.ed, .gr, .y){
      if( a$data$misc$resType == "tunif" ){ # Comparing with uniform density
        d <- sqrt(.ed) - 1
      } else { # Comparing with Gaussian
        d <- dnorm(.gr, 0, sd = ifelse(a$data$misc$resType == "tnormal", 1, sd(.y)))
        d <- sqrt(.ed) - sqrt(d)
      }
      return( sign(d) * abs(d) ^ (1/3) )
    }
  }
  
  ### 2. Density Estimation 
  # Compute joint or conditional residual density
  yv <- as.vector(a$data$res$y)
  M <- cbind(rep(a$data$res$x, length(yv) / nrow(a$data$res)), yv)

  estYcX <- .fastKernDens(dat = M, xlimit = NULL, ylimit = NULL,
                          cond = TRUE, bw = xtra$bw, ngr = xtra$n, tol = xtra$tol)$dXY
  
  ### 3. Plotting
  a$data <- data.frame("z" = xtra$dFun(.ed=as.numeric(t(estYcX$fhat)), 
                                       .gr=estYcX$x2, .y=yv), 
                       "x" = rep(estYcX$x1, each=xtra$n[1]), 
                       "y" = rep(estYcX$x2, xtra$n[2]))
  a$mapping <- aes(x = x, y = y, fill = z)
  a$inherit.aes <- FALSE
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE }
  
  # Build layers
  out <- list()
  out[[1]] <- do.call("geom_raster", a) 
  out[[2]] <- scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) 

  class(out) <- "listOfLayers"
  
  return( out )
  
}

