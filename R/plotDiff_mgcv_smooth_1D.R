#'
#' Plotting differences between 1D smooths
#' 
#' @description Plotting differences between 1D smooths.
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
#' 
plotDiff.mgcv.smooth.1D <- function(s1, s2, n = 100, trans = identity, unconditional = FALSE){
  
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

