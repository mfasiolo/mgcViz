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
#'             when calculating the ALE effects.
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
#' @importFrom stats model.matrix ecdf cmdscale formula
#' @name ALE.gam
#' @rdname ALE.gam
#' @export ALE.gam
#' @export
#'
ALE.gam <- function(o, x, newdata = NULL, type = "link", nbin = 40, oind = 1, center = 1, ...) {
  
  if( !(type %in% c("link", "response")) ) { stop("Argument \"type\" must be either \"link\" or \"response\"") }
  
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
  # Same for Jacobian
  jacFun <- function(.o, .d, .t, ...){
    .f <- .o$family
    # Case 1: special case where the Jacobian is full (not block-diagonal)
    if( .t == "response" && .f$family %in% c("multinom", "stackPredictiveFamily")  ){ 
      return( .multinomJacobian(.o, .d, oind, ...)  )
    } 
    # Below we cover GAM and GAMLSS (and multinom + stack only if .t == "link")
    .x <- model.matrix(.o, newdata = .d)
    .lpi <- attr(.x, "lpi")
    if( !is.null(.lpi) ){
     .x <- .x[ , .lpi[[oind]], drop = FALSE]
    }
    # Case 2: we just return the model matrix X
    if(.t == "link"){
      return( .x )
    } 
    # Case 3: we need to transform using the link function
    if( !is.null(.lpi) ){ 
      .eta <- predict(.o, newdata = .d, type = "link", ...)[ , oind]
      .x <- .x * as.vector( .o$family$linfo[[oind]]$mu.eta(.eta) )
    } else {                         
      .eta <- predict(.o, newdata = .d, type = "link", ...)
      .x <- .x * as.vector( .o$family$mu.eta(.eta) ) 
    }
    return( .x )
  }
  # Same for covariance function
  varFun <- function(.o, .t, ...){
    .V <- vcov(.o, ...)
    .lpi <- attr(.o$formula, "lpi")
    .respMulti <- (.o$family$family %in% c("multinom", "stackPredictiveFamily")) && (.t == "response")
    # If we are using multinomial parametrization AND we are on response scale, 
    # then we don't discard elements of .V (because Jacobian will be full) 
    if( !is.null(.lpi) && !.respMulti  ) {
      .V <- .V[.lpi[[oind]], .lpi[[oind]], drop = FALSE] 
    }
    return( .V )
  }
  
  out <- .prepare.ALE(o = o, xnam = x, data = data, type = type, K = nbin, predFun = predFun, 
                      jacFun = jacFun, varFun = varFun, center = center, ...)

  return( out )
  
}




