#'
#' Create Accumulated Local Effects (ALE) for GAMs
#'
#' @param o a fitted GAM model.
#' @param x the name of the variable along which we want to produce the ALE effect.
#' @param newdata optional argument indicating the data to be used to produce the ALE effect.
#'                If \code{NULL} the data contained in \code{o} will be used.
#' @param type if set to "link" (the default option) the model output will be the linear predictor, if
#'             set to "response" the model output is on the scale of the response.
#' @param nbin number of intervals into which the predictor range is divided 
#'             when calculating the ALE effects. Ignored for factor predictors of if the \code{bins} argument is provided.
#' @param bins a grid defining the interval into which the predictor should be binned. Determined
#'             automatically by default. Ignored for factor predictors.
#' @param oind relevant only when the model \code{o} has multiple linear predictors (e.g. for GAMLSS models
#'             or for \code{multinom} regression). \code{oind} is the index of the output variable used for
#'             the ALE effect (i.e., only \code{predict(o)[ , oind]}.
#' @param center if set to 0 the ALE effect is not centered and the effect is equal to zero at the smallest
#'               value on x-grid. If set to 1 (default) the effect is centered as done in Apley and Zhu, 2016.
#'               That is, an estimate of the expected value of the uncentered effect is subtracted, so the effect is 
#'               centered similarly to smooth effects in GAMs. If set to 2, the expected value of the model output
#'               at the smallest value on the x-grid is added to the uncentered effect.
#' @param ... extra arguments that will be passed to \code{predict} and \code{vcov}.
#' @return An object of class \code{ALEXD}, where \code{X} is the number of dimensions, which can be plotted 
#'         using \code{plot.ALEXD} (only \code{X=1} is provided at the moment).
#' @references Apley, D.W., and Zhu, J, 2016. Visualizing the effects of predictor variables in black 
#'             box supervised learning models. arXiv preprint arXiv:1612.08468.
#' @author Matteo Fasiolo and Christian Capezza, with some internal code having been adapted from the ALEPlot 
#'         package of Dan Apley.
#' @seealso [plot.ALE1D]
#' @examples
#' # Example using Tweedie distribution
#' library(mgcViz)
#' set.seed(3)
#' n<-400
#' ## Simulate data...
#' dat <- gamSim(1,n=n,dist="poisson",scale=.2)
#' dat$y <- rTweedie(exp(dat$f),p=1.3,phi=.5) ## Tweedie response
#' 
#' ## Fit a fixed p Tweedie, with wrong link ...
#' b <- gam(list(y~s(x0)+s(x1)+s(x2)+s(x3),~1,~1), family=twlss(), data=dat)
#' 
#' plot(ALE(b, "x2", type = "response", oind = 1))
#' 
#' # With manually chosen bins
#' plot(ALE(b, "x2", type = "response", oind = 1, 
#'          bins = c(0.1, 0.25, 0.5, 0.6, 0.9, 0.95, 0.99, 1)))
#' 
#' @importFrom stats model.matrix ecdf cmdscale formula
#' @name ALE.gam
#' @rdname ALE.gam
#' @export ALE.gam
#' @export
#'
ALE.gam <- function(o, x, newdata = NULL, type = "link", nbin = 40, bins = NULL, oind = 1, center = 1, ...) {
  
  if( !(type %in% c("link", "response")) ) { stop("Argument \"type\" must be either \"link\" or \"response\"") }
  
  if( type == "response" && o$family$family == "multinom" ){
    o$family$jacobian <- .multinomJacobian
  }
  
  data <- if( !is.null(newdata) ){
    newdata
  } else {
    if( inherits(o, "gamViz") && !is.null(o$store$newdata) ) { o$store$newdata } else { o$model }
  }
  
  if( !(x %in% names(data)) ) { stop("(x %in% names(data)) == FALSE") }
  
  # NB we are fixing "oind" at this points
  predFun <- function(.o, .d, .t, ...) {
    .mu <- as.matrix( predict(.o, newdata = .d, type = .t, ...) )
    return( .mu[ , oind] )
  }
  # Same for Jacobian: this returns derivative of output w.r.t. beta (and extra parameters, theta)
  jacFun <- function(.o, .d, .t, ...){
    .f <- .o$family
    # Case 1: if family provides a Jacobian function, we use that one (via a wrapper)
    if( .t == "response" && !is.null(.f$jacobian)  ){ 
      return( .jacobian_wrap(.o, .d, oind, ...)  )
    } 
    .x <- model.matrix(.o, newdata = .d)
    .lpi <- attr(.o$formula, "lpi")
    if( !is.null(.lpi) ){
     .x <- .x[ , .lpi[[oind]], drop = FALSE]
    }
    # Case 2: we just return the model matrix X
    if(.t == "link"){
      return( list("J" = .x) )
    } 
    # Case 3: we need to transform using the link function
    .eta <- predict(.o, newdata = .d, type = "link", ...)
    if( !is.null(.lpi) ){ 
      .x <- .x * as.vector( .o$family$linfo[[oind]]$mu.eta(.eta[ , oind]) )
    } else {                         
      .x <- .x * as.vector( .o$family$mu.eta(.eta) ) 
    }
    return( list("J" = .x) )
  }
  # Same for covariance function
  varFun <- function(.o, .t, .J, ...){
    .V <- vcov(.o, ...)
    .lpi <- attr(.o$formula, "lpi")
    # If TRUE, we are dealing with multiple linear predictors. The output of interest (mu)
    # does not depend on all the parameters if:
    if( !is.null(.lpi) ){ 
        if( !is.null(.J$lpi) ){ # A) The Jacobian provides an lpi index OR...
          .lpi <- c(unlist(.J$lpi), .J$theta_idx)
        } else {                
          if( is.null(.o$family$jacobian) || type == "link" ){ # B) We are in the standard GAMLSS case and/or predict on link scale
            .lpi <- .lpi[[oind]]
          }
      }                         # So we keep only some elements of V
      .V <- .V[.lpi, .lpi, drop = FALSE]
    }
    return( .V )
  }
  
  out <- .prepare.ALE(o = o, xnam = x, data = data, type = type, K = nbin, bins = bins, predFun = predFun, 
                      jacFun = jacFun, varFun = varFun, center = center, ...)

  return( out )
  
}




