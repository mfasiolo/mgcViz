########
#' Fit a QGAM model and get a gamViz object
#' 
#' @description These are wrapper that fits a QGAM model using [qgam::qgam] and 
#'              converts it to a \code{gamViz} object using the [getViz] function.
#'              It is essentially a shortcut.
#'              
#' @param form,data,qu,lsig,err same arguments as in [qgam::qgam].
#' @param aQgam list of further arguments to be passed to [qgam::qgam].
#' @param aViz list of arguments to be passed to [getViz].
#' @return An object of class "gamViz" which can, for instance, be plotted using [plot.gamViz].
#' @name qgamV
#' @examples 
#' library(mgcViz)
#' set.seed(2) ## simulate some data...
#' dat <- gamSim(2,n=1000,dist="normal",scale=0.25)$data
#' 
#' # Fit GAM and get gamViz object
#' b <- qgamV(y~s(x) + s(z) + I(x*z), data = dat, qu = 0.2,
#'            aQgam = list(argGam = list(select = TRUE)), aViz = list("nsim" = 0))
#' 
#' # This is equivalent to doing
#' # 1. Fit QGAM
#' # b <- qgam(y~s(x) + s(z) + I(x*z), data=dat, qu = 0.2, argGam = list(select = TRUE))
#' # 2. Convert to gamViz object
#' # b <- getViz(b, nsim = 0)
#' 
#' # Either way, we all effects by doing
#' print(plot(b, allTerms = TRUE), pages = 1)
#'
#' @rdname qgamV
#' @export qgamV
#
qgamV <- function(form, data, qu, lsig = NULL, err = 0.05,  aQgam = list(), aViz = list()){
  
  obj <- do.call("qgam", c(list("form" = form, "qu" = qu, 
                               "data" = data, "lsig" = lsig, "err" = err), aQgam))
  
  obj <- do.call("getViz", c(list("o" = obj), aViz))
  
  return( obj )
  
}