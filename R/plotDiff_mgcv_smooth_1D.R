#'
#' Plotting differences between two 1D smooth effects
#' 
#' @description This method can be used to plot the difference between two 1D
#'              smooth effects. Mainly meant to be used with by-factor smooths.
#' @param s1 a smooth effect object, extracted using [mgcViz::sm].
#' @param s2 another smooth effect object.
#' @param n number of grid points used to compute main effects and c.i. lines. 
#'          For a nice smooth plot this needs to be several times the estimated degrees of 
#'          freedom for the smooth.
#' @param trans monotonic function to apply to the smooth and residuals, before plotting.
#'              Monotonicity is not checked. 
#' @param unconditional if \code{TRUE} then the smoothing parameter uncertainty corrected covariance 
#'                      matrix is used to compute uncertainty bands, if available.
#'                      Otherwise the bands treat the smoothing parameters as fixed.
#' @param ... currently unused.
#' @return An objects of class \code{plotSmooth}.
#' @details Let sd be the difference between the fitted smooths, that is: sd = s1 - s2.
#'          sd is a vector of length n, and its covariance matrix is 
#'          Cov(sd) = X1\%*\%Sig11\%*\%t(X1) + X2\%*\%Sig22\%*\%t(X2) - X1\%*\%Sig12\%*\%t(X2) - X2\%*\%Sig12\%*\%t(X1), 
#'          where: X1 (X2) and Sig11 (Sig22) are the design matrix and the covariance matrix 
#'          of the coefficients of s1 (s2), while Sig12 is the cross-covariance matrix between
#'          the coefficients of s1 and s2. To get the confidence intervals we need only diag(Cov(sd)), 
#'          which here is calculated efficiently (without computing the whole of Cov(sd)).        
#' @references Marra, G and S.N. Wood (2012) Coverage Properties of Confidence Intervals for 
#'             Generalized Additive Model Components. Scandinavian Journal of Statistics.
#' @name plotDiff.mgcv.smooth.1D
#' @examples 
#' # Simulate data and add factors uncorrelated to the response
#' library(mgcViz)
#' set.seed(6898)
#' dat <- gamSim(1,n=1500,dist="normal",scale=20)
#' dat$fac <- as.factor( sample(c("A1", "A2", "A3"), nrow(dat), replace = TRUE) ) 
#' dat$logi <- as.logical( sample(c(TRUE, FALSE), nrow(dat), replace = TRUE) ) 
#' bs <- "cr"; k <- 12
#' b <- gam(y ~ s(x2,bs=bs,by = fac), data=dat)
#' o <- getViz(b, nsim = 0)
#' 
#' # Extract the smooths correspoding to "A1" and "A2" and plot their differences
#' # with credible intervals
#' plotDiff(s1 = sm(o, 1), s2 = sm(o, 2)) + l_ciPoly() + 
#'   l_fitLine() + geom_hline(yintercept = 0, linetype = 2)
#'   
#' @rdname plotDiff.mgcv.smooth.1D
#' @export plotDiff.mgcv.smooth.1D
#' @export
#' 
plotDiff.mgcv.smooth.1D <- function(s1, s2, n = 100, trans = identity, unconditional = FALSE, ...){
  
  gObj <- s1$gObj
  smo1 <- gObj$smooth[[ s1$ism ]]
  smo2 <- gObj$smooth[[ s2$ism ]]
  
  if( smo1$by == "NA" || smo2$by == "NA" ){
    warning("This is guaranteed to work only when differencing by-factor smooths")
  }
  
  # Use Bayesian cov matrix including smoothing parameter uncertainty?
  if (unconditional) { 
    if ( is.null(gObj$Vc) ) { 
      warning("Smoothness uncertainty corrected covariance not available") 
    } else { 
      V <- gObj$Vc 
    } 
  } else {
    V <- gObj$Vp
  }
  
  # 1) Get X and coeff for both smooth
  P1 <- .plotDiffFit(sm = smo1, gObj = gObj, n = n)
  P2 <- .plotDiffFit(sm = smo2, gObj = gObj, n = n)
  
  # Subset the covariance matrix so we look only at relevant entries
  V <- V[c(P1$crange, P2$crange), c(P1$crange, P2$crange)]
  
  # Covariance matrix of differences is cbind(X1, -X2) %*% V %*% rbind(X1, -X2)  
  P1$fit <- P1$fit - P2$fit
  X <- cbind(P1$X, - P2$X)
  P1$se <- sqrt( rowSums( (X %*% V) * X ) )
  P1$ylab <- paste(P1$ylab, "-", P2$ylab)
  P1$raw <- NULL
  
  out <- .plot.mgcv.smooth.1D(x = NULL, P = P1, trans = trans, maxpo = NULL)
  
  class(out) <- c("plotSmooth", "gg")
  
  return(out)
  
}

