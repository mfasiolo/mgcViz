#'
#' Plotting Markov random field smooths
#' 
#' @description This is the plotting method for Markov random field smooths.
#' @param x a smooth effect object, extracted using [mgcViz::sm].
#' @param trans monotonic function to apply to the smooth and residuals, before plotting.
#'              Monotonicity is not checked. 
#' @param unconditional if \code{TRUE} then the smoothing parameter uncertainty corrected covariance 
#'                      matrix is used to compute uncertainty bands, if available.
#'                      Otherwise the bands treat the smoothing parameters as fixed.
#' @param seWithMean if TRUE the component smooths are shown with confidence intervals that 
#'                   include the uncertainty about the overall mean. If FALSE then the uncertainty
#'                   relates purely to the centred smooth itself. Marra and Wood (2012) suggests 
#'                   that TRUE results in better coverage performance, and this is also suggested 
#'                   by simulation.
#' @param ... currently unused.
#' @return An objects of class \code{plotSmooth}.
#' @references Marra, G and S.N. Wood (2012) Coverage Properties of Confidence Intervals for 
#'             Generalized Additive Model Components. Scandinavian Journal of Statistics.
#' @name plot.mrf.smooth
#' @examples
#' library(mgcViz)
#' ## Load Columbus Ohio crime data (see ?columbus for details and credits)
#' data(columb)       ## data frame
#' data(columb.polys) ## district shapes list
#' xt <- list(polys=columb.polys) ## neighbourhood structure info for MRF
#' par(mfrow=c(2,2))
#' ## First a full rank MRF...
#' b <- gam(crime ~ s(district,bs="mrf",xt=xt),data=columb,method="REML")
#' b <- getViz(b) 
#' 
#' # Manual plot
#' plot(sm(b, 1)) + l_poly(colour = 2) + 
#'   scale_fill_gradientn(colours = heat.colors(50))
#' 
#' # Default plot 
#' plot(b)
#' 
#' @rdname plot.mrf.smooth
#' @export plot.mrf.smooth
#' @export
#' 
plot.mrf.smooth <- function(x, trans = identity, 
                            seWithMean = FALSE, unconditional = FALSE, ...) {
  
  # 1) Prepare data
  P <- .prepareP(o = x, unconditional = unconditional, residuals = TRUE, 
                 resDen = "none", se = TRUE, se.mult = 1, n = NULL, n2 = NULL,  
                 xlab = NULL, ylab = NULL, main = NULL, ylim = NULL, xlim = NULL,
                 too.far = NULL, seWithMean = seWithMean)
  
  # 2) Produce output object
  out <- .plot.mrf.smooth(x = P$smooth, P = P, trans = trans)
  
  class(out) <- c("plotSmooth", "gg")
  
  return(out)
  
}


############### Internal function
#' @noRd
.plot.mrf.smooth <- function(x, P, trans) {
  
  .dat <- list()
  
  # Build dataset on fitted effect
  .npb <- sapply(x$xt$polys, nrow)
  .dat$fit <- as.data.frame( do.call("rbind", x$xt$polys) )
  names(.dat$fit) <- c("x", "y")
  .dat$fit$z <- rep(P$fit, .npb) 
  .dat$fit$tz <- trans( .dat$fit$z )
  .dat$fit$id <- rep(P$raw, .npb) 
  
  .dat$misc <- list("trans" = trans)
  
  .pl <- ggplot(data = .dat$fit, aes(x = x, y = y, fill = tz, group = id)) +
         labs(title = P$main, x = "x", y = "y") +
         theme_bw() +
         theme(panel.grid.major = element_blank(), 
               panel.grid.minor = element_blank())
  
  return( list("ggObj" = .pl, "data" = .dat, type = "mrf") )
  
}
