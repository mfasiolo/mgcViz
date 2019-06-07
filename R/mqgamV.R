########
#' Fit multiple QGAM models and get a mgamViz object
#' 
#' @description These are wrapper that fits multple QGAM models using [qgam::mqgam] and 
#'              converts it to a \code{mgamViz} object using the [getViz] function.
#'              It is essentially a shortcut.
#'              
#' @param form,data,qu,lsig,err same arguments as in [qgam::mqgam].
#' @param aQgam list of further arguments to be passed to [qgam::mqgam].
#' @param aViz list of arguments to be passed to [getViz].
#' @return An object of class "mgamViz" which can, for instance, be plotted using [plot.mgamViz].
#' @name mqgamV
#' @importFrom utils packageVersion
#' @examples 
#' library(mgcViz)
#' set.seed(2) ## simulate some data...
#' dat <- gamSim(2,n=500,dist="normal",scale=0.25)$data
#' 
#' # Fit GAM and get gamViz object
#' b <- mqgamV(y~s(x) + s(z) + I(x*z), data = dat, qu = c(0.25, 0.5, 0.75),
#'             aQgam = list(argGam = list(select = TRUE)), aViz = list("nsim" = 0))
#' 
#' # This is equivalent to doing
#' # 1. Fit QGAM
#' # b <- mqgam(y~s(x) + s(z) + I(x*z), data=dat, 
#' #            qu = c(0.25, 0.5, 0.75), argGam = list(select = TRUE))
#' # 2. Convert to gamViz object
#' # b <- getViz(b, nsim = 0)
#' 
#' # Either way, we all effects by doing
#' print(plot(b, allTerms = TRUE), pages = 1)
#' 
#'
#' @rdname mqgamV
#' @export mqgamV
#
mqgamV <- function(form, data, qu, lsig = NULL, err = NULL,  aQgam = list(), aViz = list()){
  
  if( is.null(err) && packageVersion("qgam") < "1.3.0" ) {  
    err <- 0.05
  }
  
  obj <- do.call("mqgam", c(list("form" = form, "qu" = qu, 
                                 "data" = data, "lsig" = lsig, "err" = err), aQgam))
  
  obj <- do.call("getViz", c(list("o" = obj), aViz))
  
  return( obj )
  
}