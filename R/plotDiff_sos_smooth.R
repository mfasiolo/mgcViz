#'
#' Plotting differences between smooths of sphere
#' 
#' @description Plotting differences between smooths of sphere.
#' @name plotDiff.sos.smooth
#' @examples 
#' #### 1) Simulate data and add factors uncorrelated to the response
#' library(mgcViz)
#' set.seed(0)
#' n <- 500
#' 
#' f <- function(la,lo) { ## a test function...
#'   sin(lo)*cos(la-.3)
#' }
#' 
#' ## generate with uniform density on sphere...  
#' lo <- runif(n)*2*pi-pi ## longitude
#' la <- runif(3*n)*pi-pi/2
#' ind <- runif(3*n)<=cos(la)
#' la <- la[ind];
#' la <- la[1:n]
#' 
#' ff <- f(la,lo)
#' y <- ff + rnorm(n)*.2 ## test data
#' 
#' ## generate data for plotting truth...
#' lam <- seq(-pi/2,pi/2,length=30)
#' lom <- seq(-pi,pi,length=60)
#' gr <- expand.grid(la=lam,lo=lom)
#' fz <- f(gr$la,gr$lo)
#' zm <- matrix(fz,30,60)
#' 
#' dat <- data.frame(la = la *180/pi,lo = lo *180/pi,y=y)
#' dat$fac <- as.factor( sample(c("A1", "A2", "A3"), nrow(dat), replace = TRUE) ) 
#' 
#' #### 2) fit spline on sphere model...
#' bp <- gam(y~s(la,lo,bs="sos",k=60, by = fac),data=dat)
#' bp <- getViz(bp)
#' 
#' # Extract the smooths correspoding to "A1" and "A3" and plot their difference
#' # Using scheme = 0 
#' plotDiff(s1 = sm(bp, 1), s2 = sm(bp, 3)) + l_fitRaster() + 
#'   l_fitContour() + l_coordContour() + l_bound()
#' 
#' # Using scheme = 1
#' plotDiff(s1 = sm(bp, 1), s2 = sm(bp, 2), scheme = 1) + l_fitRaster() + l_fitContour() 
#' @rdname plotDiff.sos.smooth
#' @export plotDiff.sos.smooth
#' 
plotDiff.sos.smooth <- function(s1, s2, n = 40, too.far = 0.1, phi = 30, theta = 30, 
                                scheme = 0, trans = identity, unconditional = FALSE){
  
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
  P1 <- .plotDiffFit(sm = smo1, gObj = gObj, n = n, too.far = too.far, 
                     scheme = scheme, phi = phi, theta = theta)
  P2 <- .plotDiffFit(sm = smo2, gObj = gObj, n = n, too.far = too.far, 
                     scheme = scheme, phi = phi, theta = theta)
  
  #str(P1$se)
  
  # Subset the covariance matrix so we look only at relevant entries
  V <- V[c(P1$crange, P2$crange), c(P1$crange, P2$crange)]
  
  # Covariance matrix of differences is cbind(X1, -X2) %*% V %*% rbind(X1, -X2)  
  P1$fit <- P1$fit - P2$fit
  X <- cbind(P1$X, - P2$X)
  P1$se <- sqrt( rowSums( (X %*% V) * X ) )
  P1$main <- paste(P1$main, "-", P2$main)
  P1$raw <- NULL
  
  # 2) Produce output object
  if(scheme == 0){ # plot on sphere
    
    out <- .plot.sos.smooth(x = NULL, P = P1, trans = trans, maxpo = NULL)
    out$type <- "sos0"
    
  } else { # standard 2D plot
    
    out <- .plot.mgcv.smooth.2D(x = NULL, P = P1, trans = trans, maxpo = NULL)
    out$type <- "sos1"

  }
  
  class(out) <- c("plotSmooth", "gg")
  
  return(out)
  
}
