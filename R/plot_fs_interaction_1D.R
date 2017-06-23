#' Plotting one dimensional smooth factor interactions
#' 
#' @description XXX
#' @name plot.fs.interaction.1D
#' @examples 
#' library(mgcViz)
#' @rdname plot.fs.interaction.1D
#' @export plot.fs.interaction.1D
plot.fs.interaction.1D <- function(o, residuals=FALSE,rug=TRUE,se=TRUE,n=100,
                                   jit=FALSE,xlab=NULL,ylab=NULL,main=NULL,
                                   ylim=NULL,xlim=NULL,shade=FALSE,shade.col=I("gray80"),
                                   shift=0,trans=I,seWithMean=FALSE,unconditional=FALSE,by.resids=FALSE,
                                   scheme=0, ...)
{
  if (length(scheme)>1){ 
    scheme <- scheme[1]
    warning( "scheme should be a single number" )
  }
  
  o$smooth <- o$gObj$smooth[[o$ism]]
 
  # This creates/modifies variables in the environment.
  # INPUTS: unconditional, o, residuals, se
  # NEW/MODIFIED VARIABLES: o, w.resid, partial.resids, se2.mult, se1.mult, se, fv.terms, order  
  fv.terms <- o$store$termsFit[ , o$store$np + o$ism]
  eval( .initializeXXX )
  
  # Prepare for plotting
  tmp <- .createP(sm=o$smooth, x=o$gObj, partial.resids=partial.resids,
                  rug=rug, se=se, scale=FALSE, n=n, n2=NULL,
                  pers=NULL, theta=NULL, phi=NULL, jit=jit, xlab=xlab, ylab=ylab, main=main, label=term.lab,
                  ylim=ylim, xlim=xlim, too.far=NULL, shade=shade, shade.col=shade.col,
                  se1.mult=se1.mult, se2.mult=se2.mult, shift=shift, trans=trans,
                  by.resids=by.resids, scheme=scheme, seWithMean=seWithMean, fitSmooth=fv.terms,
                  w.resid=w.resid, ...)
  pd <- tmp[["P"]]
  attr(o$smooth, "coefficients") <- tmp[["coef"]]
  rm(tmp)
  
  # Plotting
  .ggobj <- .plot.fs.interaction.1D(x=o$smooth, P=pd,  ylim = ylim, shift=shift, trans=trans, 
                                    by.resids=by.resids, scheme=scheme, ...)
  
  .ggobj <- .ggobj+theme_bw()
  
  attr(.ggobj, "rawData") <- pd
  .ggobj
}

# Internal function
.plot.fs.interaction.1D <- function(x, P=NULL, ylim=NULL, shift=0, trans=I, by.resids=FALSE, scheme=0, ...) {
  
  .dat <- data.frame("x"=rep(P$x,P$nf), "y"=trans(P$fit+shift), "id"=as.factor(rep(x$flev, each=P$n)))
  
  if(is.null(ylim)) ylim <- range(.dat$y)
  
  .pl <- ggplot(data=.dat, aes("x"=x, "y"=y, "colour"=id)) + geom_line() + 
                labs(title = P$main, x = P$xlab, y = P$ylab) + ylim(ylim[1], ylim[2])
  
  return( .pl )
} ## end plot.fs.interaction

