#' Plotting one dimensional smooth effects
#' 
#' @description XXX
#' @name plot.mgcv.smooth.1D
#' @examples 
#' library(mgcViz)
#' n <- 1e3
#' x1 <- rnorm(n); x2 <- rnorm(n)
#' dat <- data.frame("x1"=x1, "x2"=x2, "y"=sin(x1) + 0.5*x2^2 + pmax(x2, 0.2)*rnorm(n))
#' b <- bam(y ~ s(x1)+s(x2), data=dat, method="fREML", discrete = T)
#' 
#' v <- getViz(b)
#' plot(v(1), rug=T, resDen="cond", residuals=T)
#' @rdname plot.mgcv.smooth.1D
#' @export plot.mgcv.smooth.1D
plot.mgcv.smooth.1D <- function(o, residuals=FALSE,rug=TRUE,se=TRUE,n=100,
                                jit=FALSE,xlab=NULL,ylab=NULL,main=NULL,
                                ylim=NULL,xlim=NULL,shade=FALSE,shade.col=I("gray80"),
                                shift=0,trans=I,seWithMean=FALSE,unconditional=FALSE,by.resids=FALSE,
                                scheme=0, resDen="none", ngr = c(50, 50), bw = NULL, tol = 1e-6, alpDen = 0.7, 
                                dTrans = NULL, paletteDen = viridis(50, begin=0.2), ...)
{
  if (length(scheme)>1){ 
    scheme <- scheme[1]
    warning( "scheme should be a single number" )
  }
  
  resDen <- match.arg(resDen, c("none", "cond", "joint"))
  
  o$smooth <- o$gObj$smooth[[o$ism]]
  
  # This creates/modifies variables in the environment.
  # INPUTS: unconditional, o, residuals, se, resDen 
  # NEW/MODIFIED VARIABLES: o, w.resid, partial.resids, se2.mult, se1.mult, se, fv.terms, order 
  fv.terms <- o$store$termsFit[ , o$store$np + o$ism]
  init <- .initializeXXX(o, unconditional, residuals, resDen, se, fv.terms)
  # affect initialize output
  o <- init$o
  w.resid <- init$w.resid
  partial.resids <- init$partial.resids
  se2.mult <- init$se2.mult
  se1.mult <- init$se1.mult
  se <- init$se
  fv.terms <- init$fv.terms
  order <- init$order
  
  # Prepare for plotting
  tmp <- .createP(sm=o$smooth, x=o$gObj, partial.resids=partial.resids,
                  rug=rug, se=se, scale=FALSE, n=n, n2=NULL,
                  pers=NULL, theta=NULL, phi=NULL, jit=jit, xlab=xlab, ylab=ylab, main=main, label=term.lab,
                  ylim=ylim, xlim=xlim, too.far=NULL, shade=shade, shade.col=shade.col,
                  se1.mult=se1.mult, se2.mult=se2.mult, shift=shift, trans=trans,
                  by.resids=by.resids, scheme=scheme, seWithMean=seWithMean, fitSmooth=fv.terms,
                  w.resid=w.resid, resDen=resDen, ...)
  pd <- tmp[["P"]]
  attr(o$smooth, "coefficients") <- tmp[["coef"]]
  rm(tmp)
  
  # Plotting
  .ggobj <- .plot.mgcv.smooth.1D(x=o$smooth, P=pd, partial.resids=partial.resids, rug=rug, se=se, scale=FALSE, n=n,
                                 jit=jit, shade=shade||(scheme==1), shade.col=shade.col, ylim = ylim,
                                 shift=shift, trans=trans, by.resids=by.resids, resDen=resDen, ngr=ngr, bw=bw, tol=tol, alpDen=alpDen, 
                                 dTrans=dTrans, paletteDen=paletteDen, ...)
  
  .ggobj <- .ggobj + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  
  attr(.ggobj, "rawData") <- pd
  .ggobj
}


# Internal function for plotting one dimensional smooths
.plot.mgcv.smooth.1D <- function(x, P, partial.resids=FALSE, rug=TRUE, se=TRUE, scale=TRUE, n=100,
                                 jit=FALSE, shade=FALSE, shade.col=I("gray80"), ylim = NULL,
                                 shift=0, trans=I, by.resids=FALSE, scheme=0, resDen="none", 
                                 ngr = c(50, 50), bw = NULL, tol = 1e-6, alpDen = 0.7, 
                                 dTrans = NULL, paletteDen = viridis(50, begin=0.2), ...)
{
  if (scheme == 1){ shade <- TRUE }
  
  ul <- P$fit + P$se ## upper CL
  ll <- P$fit - P$se ## lower CL  
  
  if (scale==FALSE && is.null(ylim)) { # Calculate ylim of plot
    ylimit <- range(c(if(partial.resids||(resDen!="none")){ P$p.resid } else { P$fit }, if(se){ c(ul, ll) } else { c() }), na.rm = TRUE) 
  }
  
  ylimit <- if (is.null(ylim)){ ylimit <- ylimit + shift } else { ylim }
  
  .pl <- ggplot(data=data.frame(x=P$x, y=trans(P$fit+shift), uci=trans(ul+shift), lci=trans(ll-shift)), aes(x=x, y=y)) + 
    xlim(P$xlim[1], P$xlim[2]) + ylim(trans(ylimit[1]), trans(ylimit[2])) + labs(title = P$main, x = P$xlab, y = P$ylab)
  
  if( resDen != "none" ){ # Plot conditional residual density
    if( is.null(dTrans) ){ dTrans <- function(.x){ .x^(1/3) } }
    
    .datR <- cbind(P$raw, trans(P$p.resid+shift))
    # Suppress warnings related to ngrid being too small relative to bw. Happens with big dataset.
    withCallingHandlers({
      if( is.null(bw) ){ bw <- c(dpik(.datR[ , 1], range.x=P$xlim, gridsize=ngr[1]), 
                                 dpik(.datR[ , 2], range.x=ylimit, gridsize=ngr[2])) }
      
      estXY <- bkde2D(.datR, range.x = list(P$xlim, ylimit), gridsize = ngr, bandwidth = bw)
      
      if( resDen == "cond" ){ # Calculate conditional density of residuals | x
        estXY$fhat <- estXY$fhat / bkde(.datR[ , 1], gridsize = ngr[1], range.x = P$xlim, bandwidth = bw[1])$y 
      }
    }, warning = function(w) { invokeRestart("muffleWarning") })
    
    estXY$fhat[ estXY$fhat <= tol*dnorm(0, 0, sd(.datR[ , 2])) ] <- NA 
    
    cden <- data.frame("d" = sqrt(as.numeric(t(estXY$fhat))), 
                       "x" = rep(estXY$x1, each=ngr[1]), 
                       "y" = rep(estXY$x2, ngr[2]))
    .pl <- .pl + geom_raster(data=cden, aes(x=x, y=y, fill=d), inherit.aes=FALSE, alpha = alpDen) + 
      scale_fill_gradientn(colours = paletteDen, na.value="white")
  }
  
  # Add shade or lines for confidence bands
  if(se){
    if( shade ){ 
      .pl <- .pl + geom_polygon(data = data.frame("x"= c(P$x,P$x[n:1],P$x[1]), "y" = trans(c(ul,ll[n:1],ul[1])+shift)), 
                                aes(x = x, y = y, fill = shade.col), inherit.aes = F)
    } else {
      .tmpF <- function(pl, ..., linetype="dashed") # Alter default "linetype"
      {
        pl <- pl + geom_line(aes(x, uci), linetype=linetype, ...) + geom_line(aes(x, lci), linetype=linetype, ...)
      }
      .pl <- .tmpF(.pl, ...)
    }
  }
  
  # Add partial residuals
  if (partial.resids&&(by.resids||x$by=="NA")) { 
    if (length(P$raw)==length(P$p.resid)) {
      .tmpF <- function(..., shape = '.', col = "black") # Alter default shape and col
      {
        geom_point(data = data.frame(resx = P$raw, resy = trans(P$p.resid+shift)), 
                   aes(x = resx, y = resy), na.rm = TRUE, shape = shape, col = col, ...)
      }
      .pl <- .pl + .tmpF(...)
    } else {
      warning("Partial residuals do not have a natural x-axis location for linear functional terms")
    }
  }
  
  # Add rug
  if (rug) { 
    .df <- data.frame(x = if(jit){ jitter(as.numeric(P$raw)) } else { as.numeric(P$raw) } )
    .tmpF <- function(pl, ..., size = 0.2) # Alter default "size"
    {
      geom_rug(data = .df, aes(x = x), inherit.aes = F, size = size, ...)
    }
    .pl <- .pl + .tmpF(.pl, ...)
  } 
  
  .pl <- .pl + geom_line(...)
  
  return( .pl )
} 
