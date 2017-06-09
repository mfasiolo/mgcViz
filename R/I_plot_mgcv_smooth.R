
.plot.mgcv.smooth <- function(x, P, data=NULL, label="", se1.mult=1, se2.mult=2,
                               partial.resids=FALSE, rug=TRUE, se=TRUE, scale=TRUE, n=100, n2=40,
                               pers=FALSE, theta=30, phi=30, jit=FALSE, xlab=NULL, ylab=NULL, main=NULL,
                               ylim=NULL, xlim=NULL, too.far=0.1, shade=FALSE, shade.col="gray80",
                               shift=0, trans=I, by.resids=FALSE, scheme=0, hcolors=heat.colors(50),
                               contour.col=3, inter = FALSE, ...) {

  if(x$dim == 1){
    out <- .plotSmooth1D(x=x, P=P, partial.resids=partial.resids, rug=rug, se=se, scale=scale, n=n,
                         jit=jit, shade=shade||(scheme==1), shade.col=shade.col, ylim = ylim,
                         shift=shift, trans=trans, by.resids=by.resids, inter=inter, ...)
  } else {
    if(x$dim == 2){
      out <- .plotSmooth2D(x=x, P=P, data=data, label=label, se.mult=se1.mult,
                           partial.resids=partial.resids, rug=rug, se=se, scale=scale, n=n, n2=n2,
                           jit=jit, xlab=xlab, ylab=ylab, main=main,
                           ylim=ylim, xlim=xlim, shade=shade, shade.col=shade.col,
                           shift=shift, trans=trans, by.resids=by.resids, scheme=scheme, inter=inter, ...)
    } else {
      if(!is.null(P)){ warning("no automatic plotting for smooths of more than one variable") } else { return(NULL) }
    }
  }
  
  return( out )
} 

