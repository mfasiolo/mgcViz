#'
#' Plotting differences between 2D smooths
#' 
#' @description Plotting differences between 2D smooths.
#' @name plotDiff.mgcv.smooth.2D
#' @examples 
#' # Simulate data and add factors uncorrelated to the response
#' library(mgcViz)
#' set.seed(235)
#' dat <- gamSim(1,n=1500,dist="normal",scale=20)
#' dat$fac <- as.factor( sample(c("A1", "A2", "A3"), nrow(dat), replace = TRUE) ) 
#' dat$logi <- as.logical( sample(c(TRUE, FALSE), nrow(dat), replace = TRUE) ) 
#' bs <- "cr"; k <- 12
#' b <- gam(y ~ s(x2, x1, by = fac), data=dat)
#' o <- getViz(b, nsim = 0)
#' 
#' # Extract the smooths correspoding to "A1" and "A2" and plot their difference
#' pl <- plotDiff(s1 = sm(o, 1), s2 = sm(o, 2))
#' pl + l_fitRaster() + l_fitContour()
#' 
#' # Plot p-values for differences between the two smooths
#' pl + l_pvRaster() + l_pvContour()
#' 
#' @rdname plotDiff.mgcv.smooth.2D
#' @export plotDiff.mgcv.smooth.2D
#' 
plotDiff.mgcv.smooth.2D <- function(s1, s2, n = 40, too.far = 0.1, 
                                    trans = identity, unconditional = FALSE){
  
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
  P1 <- .plotDiffFit(sm = smo1, gObj = gObj, n = n, too.far = too.far)
  P2 <- .plotDiffFit(sm = smo2, gObj = gObj, n = n, too.far = too.far)
  
  # Subset the covariance matrix so we look only at relevant entries
  V <- V[c(P1$crange, P2$crange), c(P1$crange, P2$crange)]
  
  # Covariance matrix of differences is cbind(X1, -X2) %*% V %*% rbind(X1, -X2)  
  P1$fit <- P1$fit - P2$fit
  X <- cbind(P1$X, - P2$X)
  P1$se <- sqrt( rowSums( (X %*% V) * X ) )
  P1$main <- paste(P1$main, "-", P2$main)
  P1$raw <- NULL
  
  out <- .plot.mgcv.smooth.2D(x = NULL, P = P1, trans = trans, maxpo = NULL)
  
  class(out) <- c("plotSmooth", "gg")
  
  return(out)
  
}
