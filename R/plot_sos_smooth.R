#' Plotting smooths on the sphere
#' 
#' @description XXX
#' @name plot.sos.smooth
#' @param scheme if 0 the smooth effect is plot on the sphere. If 1 the smooth effect is plotted
#'               on the two hemispheres.
#' @examples 
#' library(mgcViz)
#' set.seed(0)
#' n <- 400
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
#' require(mgcv)
#' dat <- data.frame(la = la *180/pi,lo = lo *180/pi,y=y)
#' 
#' ## fit spline on sphere model...
#' bp <- gam(y~s(la,lo,bs="sos",k=60),data=dat)
#' bp <- getViz(bp)
#' 
#' # Plot on sphere
#' plot(sm(bp, 1), scheme=0) + l_fitRaster() + l_fitContour() + 
#'    l_points(shape = 19) + l_rug() + l_coordContour() + l_bound() 
#'  
#' # Plotting as in standard 2D plots
#' plot(sm(bp, 1), scheme=1) + l_fitRaster() + l_fitContour() + 
#'            l_points(shape = 19) + l_rug()
#' @rdname plot.sos.smooth
#' @export plot.sos.smooth
#' 
plot.sos.smooth <- function(x, n = 40, maxpo = 1e4, too.far = 0.1, phi = 30, theta = 30, 
                            trans = identity, scheme = 0, seWithMean = FALSE, unconditional = FALSE, ...)
{
  if (length(scheme)>1){ 
    scheme <- scheme[1]
    warning( "scheme should be a single number" )
  }
  
  # 1) Prepare data
  P <- .prepareP(o = x, unconditional = unconditional, residuals = TRUE, 
                 resDen = "none", se = TRUE, se.mult = 1, n = NULL, n2 = n,  
                 xlab = NULL, ylab = NULL, main = NULL, ylim = NULL, xlim = NULL,
                 too.far = too.far, seWithMean = seWithMean, scheme = scheme, phi = phi, theta = theta)
  
  # 2) Produce output object
  if(scheme == 0){ # plot on sphere
    
    out <- .plot.sos.smooth(x = P$smooth, P = P, trans = trans, maxpo = maxpo)
    out$type <- "sos0"

  } else { # standard 2D plot
    
    out <- .plot.mgcv.smooth.2D(x = P$smooth, P = P, trans = trans, maxpo = maxpo)
    out$type <- "sos1"
    
  }
  
  class(out) <- c("plotSmooth", "gg")
  
  return(out)
  
}


###########################
# Internal function
.plot.sos.smooth <- function(x, P, trans, maxpo) {

  .dat <- list()
  
  ### 1) Build dataset on fitted effect
  # We set to NA the entries that fall outside the globe.
  m <- length(P$xm); 
  zz <- lo <- la <- se <- rep(NA, m*m)

  se[ P$ind ] <- P$se
  zz[ P$ind ] <- P$fit
  lo[ P$ind ] <- P$lo
  la[ P$ind ] <- P$la
  
  .dat$fit <- data.frame("x" = rep(P$xm, m), 
                         "y" = rep(P$ym, each=m), 
                         "z" = zz, 
                         "tz" = trans( zz ),
                         "lo" = lo, 
                         "la" = la, 
                         "se" = se)
  
  ### 2) Build dataset on residuals
  if( !is.null(P$raw) ){
    .dat$res <- P$raw
    
    # Sample if too many points (> maxpo) 
    nres <- nrow( .dat$res )
    .dat$res$sub <- if(nres > maxpo) { 
      sample( c(rep(T, maxpo), rep(F, nres-maxpo)) )
    } else { 
      rep(T, nres) 
    }
  }
  
  .dat$misc <- list("trans" = trans)
  
  .pl <- ggplot(data = .dat$fit, aes(x = x, y = y, z = z)) +
         labs(title = P$main, x = P$xlab, y = P$ylab) + 
         theme_bw() +
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               axis.line=element_blank(), panel.border=element_blank(), 
               axis.text=element_blank(), axis.ticks=element_blank())
  
  return( list("ggObj" = .pl, "data" = .dat) )
  
}