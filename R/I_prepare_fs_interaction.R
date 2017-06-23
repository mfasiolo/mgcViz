.prepare.fs.interaction <- function(x, data=NULL, label="", se1.mult=1, se2.mult=2,
                                    partial.resids=FALSE, rug=TRUE, se=TRUE, scale=TRUE, n=100, n2=40,
                                    pers=FALSE, theta=30, phi=30, jit=FALSE, xlab=NULL, ylab=NULL, main=NULL,
                                    ylim=NULL, xlim=NULL, too.far=0.1, shade=FALSE, shade.col="gray80",
                                    shift=0, trans=I, by.resids=FALSE, scheme=0,...) {
  
  if ( x$dim>1 ){ stop("no method for base smooth dim > 1") }
  
  raw <- data[x$base$term][[1]]
  xx <- seq(min(raw),max(raw),length=n) # generate x sequence for prediction
  nf <- length(x$flev)
  fac <- rep(x$flev,rep(n,nf))
  dat <- data.frame(fac,xx)
  names(dat) <- c(x$fterm,x$base$term)
  X <- PredictMat(x,dat)
  if (is.null(xlab)) xlabel <- x$base$term else xlabel <- xlab
  if (is.null(ylab)) ylabel <- label else ylabel <- ylab
  
  return( list(X=X,scale=TRUE,se=FALSE,raw=raw,xlab=xlabel,ylab=ylabel,main="",x=xx,n=n,nf=nf) )
  
} 