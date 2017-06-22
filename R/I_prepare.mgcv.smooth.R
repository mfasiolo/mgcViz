##
## Default plot preparation method for smooth objects `x' inheriting from "mgcv.smooth"
## Input:
## `x' is a smooth object, usually part of a `gam' fit. It has an attribute
##     'coefficients' containg the coefs for the smooth, but usually these
##     are not needed.
## Output is a list of plot data containing:
##     * fit - the values for plotting 
##     * se.fit - standard errors of fit (can be NULL)
##     * the values against which to plot
##     * any raw data information
##     * any partial.residuals 

.prepare.mgcv.smooth <- function(x, data=NULL, label="", se1.mult=1, se2.mult=2,
                              partial.resids=FALSE, rug=TRUE, se=TRUE, scale=TRUE, n=100, n2=40,
                              pers=FALSE, theta=30, phi=30, jit=FALSE, xlab=NULL, ylab=NULL, main=NULL,
                              ylim=NULL, xlim=NULL, too.far=0.1, shade=FALSE, shade.col="gray80",
                              shift=0, trans=I, by.resids=FALSE, scheme=0, hcolors=heat.colors(50),
                              contour.col=3, ...) {
  
  if(x$dim == 1){
    out <- .preparePlotSmooth1D(x=x, data=data, label=label, se.mult=se1.mult, n=n, xlim = xlim, xlab=xlab, 
                                ylab=ylab, main=main, ...)
  } else {
    if(x$dim == 2){
      out <- .preparePlotSmooth2D(x=x, data=data, se.mult=se2.mult, n2=n2, label=label,
                                       xlab=xlab, ylab=ylab, main=main,
                                       ylim=ylim, xlim=xlim, too.far=too.far, ...) 
    } else {
      if(!is.null(P)){ warning("no automatic plotting for smooths of more than one variable") } else { return(NULL) }
    }
  }
  
  return( out )
} 

