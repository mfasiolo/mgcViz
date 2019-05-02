########
#' Fit a GAMM or GAMM4 model and get a gamViz object
#' 
#' @description These are wrappers that fit GAM models using [mgcv::gamm] or [gamm4::gamm4] and 
#'              convert them to a \code{gamViz} object using the [getViz] function.
#'              It is essentially a shortcut.
#'              
#' @param formula,random,family,data same arguments as in [mgcv::gamm] or [gamm4::gamm4].
#' @param method same as in [mgcv::gamm]
#' @param REML same as in [gamm4::gamm4]
#' @param aGam list of further arguments to be passed to [mgcv::gamm] or [gamm4::gamm4].
#' @param aViz list of arguments to be passed to [getViz].
#' @param keepGAMObj if \code{TRUE} a copy of the gamViz Object is kept under $gam to assure compatibility with [mgcv::gamm] and [gamm4::gamm4]. Defaults to \code{FALSE}.
#' @details WARNING: Model comparisons (e.g. with \code{anova}) should only be done using the mixed model part as described in [gamm4::gamm4]. 
#' For [mgcv::gamm] please refer to the original help file.
#' @return An object of class "gamViz" which can, for instance, be plotted using [plot.gamViz]. Also the object has the following additional elements:
#' \itemize{
#'   \item \code{lme} mixed model as in [mgcv::gamm]
#'   \item \code{mer} mixed model as in [gamm4::gamm4]
#'   \item \code{gam} a copy of the gamViz Object if setting \code{keepGAMObj = TRUE}.
#'   }
#' @name gammV
#' @examples 
#' ##### gam example
#' library(mgcViz)
#' # Simulate data
#' dat <- gamSim(1,n=400,scale=2) ## simulate 4 term additive truth
#' ## Now add 20 level random effect `fac'...
#' dat$fac <- fac <- as.factor(sample(1:20,400,replace=TRUE))
#' dat$y <- dat$y + model.matrix(~fac-1) %*% rnorm(20) * 0.5
#' 
## mgcv::gamm example
#' br <- gammV(y~s(x0)+x1+s(x2), data=dat,random=list(fac=~1))
#' summary(br)
#' plot(br)
#'
#' summary(br$lme)
#' 
#' \dontrun{
#' ## gamm4::gamm4 example
#' br4 <- gamm4V(y~s(x0)+x1+s(x2),data=dat,random=~(1|fac))
#' summary(br4)
#' plot(br4)
#' 
#' summary(br4$mer)
#' }
#' 
#' @importFrom stats gaussian
#' @importFrom gamm4 gamm4
#' @rdname gammV
#' @export gammV
#
gammV <- function(formula, random, family = gaussian(), data = list(), method = "REML", aGam = list(), aViz = list(), keepGAMObj = FALSE){

  obj <- do.call("gamm", c(list("formula" = formula, "random" = random, "family" = family, "data" = quote(data), "method" = method), aGam))

  lme <- obj$lme

  obj <- do.call("getViz", c(list("o" = obj$gam), aViz))

  if ( keepGAMObj ) { obj$gam <- obj }
  obj$lme <- lme

  return( obj )
}
