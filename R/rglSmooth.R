#' Visualizing 2D smooth effects in 3D (interactively)
#' 
#' @description XXX
#' @name rglSmooth
#' @examples 
#' # Example 1: taken from ?mgcv::te, shows how tensor pruduct deals nicely with 
#' # badly scaled covariates (range of x 5% of range of z )
#' \dontrun{
#' library(mgcViz)
#' 
#' # Simulate some data
#' test1 <- function(x,z,sx=0.3,sz=0.4) { 
#'   x <- x*20
#'   (pi**sx*sz)*(1.2*exp(-(x-0.2)^2/sx^2-(z-0.3)^2/sz^2)+
#'                  0.8*exp(-(x-0.7)^2/sx^2-(z-0.8)^2/sz^2))
#' }
#' n <- 500
#' old.par <- par(mfrow=c(2,2))
#' x <- runif(n)/20;z <- runif(n);
#' xs <- seq(0,1,length=30)/20;zs <- seq(0,1,length=30)
#' pr <- data.frame(x=rep(xs,30),z=rep(zs,rep(30,30)))
#' truth <- matrix(test1(pr$x,pr$z),30,30)
#' f <- test1(x,z)
#' y <- f + rnorm(n)*0.2
#' 
#' # Fit with t.p.r.s. basis and plot
#' b1 <- gam(y~s(x,z))
#' rglSmooth(getViz(b1)(1))
#' 
#' # Fit with tensor products basis and plot (with residuals)
#' b2 <- gam(y~te(x,z))
#' rglSmooth(getViz(b2)(1), residuals = TRUE)
#' }
#' @importFrom rgl open3d light3d surface3d axes3d title3d spheres3d aspect3d
#' @rdname rglSmooth
#' @export rglSmooth
rglSmooth <- function(o, se = TRUE, n = 40, residuals = FALSE, type = "auto", 
                      maxpo = 1e3, xlab = NULL, ylab = NULL, main = NULL, xlim = NULL, 
                      ylim = NULL,  se.mult = 1, shift = 0, trans = I, seWithMean = FALSE, 
                      unconditional = FALSE){
  
  if (type == "auto") { type <- mgcViz:::.getResTypeAndMethod(o$gObj$family$family)$type }
  
  o$smooth <- o$gObj$smooth[[o$ism]]
  resDen <- "none"
  fv.terms <- o$store$termsFit[ , o$store$np + o$ism]
  init <- mgcViz:::.initializeXXX(o, unconditional, FALSE, resDen, se, fv.terms)
  
  # Prepare for plotting
  tmp <- mgcViz:::.createP(sm = init$o$smooth, x = init$o$gObj, partial.resids = init$partial.resids,
                  se = init$se, n = NULL, n2 = n,
                  xlab = xlab, ylab = ylab, main = main,
                  ylim = ylim, xlim = xlim, too.far = 0,
                  se1.mult = init$se2.mult, se2.mult = init$se2.mult, 
                  seWithMean = seWithMean, fitSmooth = init$fv.terms,
                  w.resid = init$w.resid, resDen = resDen)#, ...) #####"!!!!!!!#########
  pd <- tmp[["P"]]
  attr(init$o$smooth, "coefficients") <- tmp[["coef"]]
  rm(tmp)
  
  if( residuals ){ 
    res <- residuals(o$gObj, type = type) 
    # Boundary checking for residuals: inefficient, but simple approach.
    if( !is.null(xlim) || !is.null(ylim) ){
      if( is.null(xlim) ) xlim <- c(-Inf, Inf)
      if( is.null(ylim) ) ylim <- c(-Inf, Inf)
      if( residuals ){
        rin <- pd$raw$x > xlim[1] & pd$raw$x < xlim[2] & pd$raw$y > ylim[1] & pd$raw$y < ylim[2]
        pd$raw <- pd$raw[rin, , drop = F]
        res <- res[rin]
      }
    }
  }

  # Subsample residuals
  if( residuals && length(res) > maxpo){
    ind <- sample(1:length(res), maxpo)
    pd$raw <- pd$raw[ind, ]
    res <- res[ ind ]
  }
  
  # New window and setup env
  open3d()
  light3d()

  # Draws non-parametric density
  surface3d(pd$x, pd$y, matrix(pd$fit, n, n), color="#FF2222", alpha=0.5)
  surface3d(pd$x, pd$y, matrix(pd$fit + 2*pd$se, n, n), 
            alpha=0.5, color="#CCCCFF",front="lines")
  surface3d(pd$x, pd$y, matrix(pd$fit - 2*pd$se, n, n), 
            alpha=0.5, color= "#CCCCFF", front="lines")
  
  # Draws the simulated data as spheres on the baseline
  if( residuals ){
   cent = min(pd$fit-3*pd$se)
   surface3d(pd$x, pd$y, matrix(cent, n, n), color="#CCCCFF",
             front = "lines", back = "lines")
   axes3d(c('x', 'y', 'z'))
   title3d(xlab = pd$xlab, zlab = pd$main, ylab = pd$ylab)
   res <- res / max(abs(res)) * max(pd$se)
   spheres3d(pd$raw$x, pd$raw$y, cent + res, 
             radius=max(c(abs(pd$fit), pd$x, pd$y))/100, 
             color= ifelse(res>0, "red", "blue"))
  } else {
    axes3d(c('x', 'y', 'z')) 
    title3d(xlab = pd$xlab, zlab = pd$main, ylab = pd$ylab)
  }
  
  aspect3d(1, 1, 1)
  
  return( invisible(NULL) )
  
}



