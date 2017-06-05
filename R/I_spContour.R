
# Function for countour plots
.spContour <- function(x,y,z,zse,xlab="",ylab="",zlab="",titleOnly=FALSE,
                       se.plot=TRUE,se.mult=1,trans=I,shift=0,...){
  ## function for contouring 2-d smooths with s.e. limits
  gap<-median(zse,na.rm=TRUE)
  zr<-max(trans(z+zse+shift),na.rm=TRUE)-min(trans(z-zse+shift),na.rm=TRUE) # plotting range
  n<-10
  while (n>1 && zr/n<2.5*gap) n<-n-1
  zrange<-c(min(trans(z-zse+shift),na.rm=TRUE),max(trans(z+zse+shift),na.rm=TRUE))
  zlev<-pretty(zrange,n)  ## ignore codetools on this one
  yrange<-range(y);yr<-yrange[2]-yrange[1]
  xrange<-range(x);xr<-xrange[2]-xrange[1]
  ypos<-yrange[2]+yr/10
  args <- as.list(substitute(list(...)))[-1]
  args$x <- substitute(x);args$y <- substitute(y)
  args$type="n";args$xlab<-args$ylab<-"";args$axes<-FALSE
  do.call("plot",args)
  
  cs<-(yr/10)/strheight(zlab);if (cs>1) cs<-1 # text scaling based on height
  
  tl<-strwidth(zlab);
  if (tl*cs>3*xr/10) cs<-(3*xr/10)/tl
  args <- as.list(substitute(list(...)))[-1]
  n.args <- names(args)
  zz <- trans(z+shift) ## ignore codetools for this
  args$x<-substitute(x);args$y<-substitute(y);args$z<-substitute(zz)
  if (!"levels"%in%n.args) args$levels<-substitute(zlev)
  if (!"lwd"%in%n.args) args$lwd<-2
  if (!"labcex"%in%n.args) args$labcex<-cs*.65
  if (!"axes"%in%n.args) args$axes <- FALSE
  if (!"add"%in%n.args) args$add <- TRUE
  do.call("contour",args)
  
  if (is.null(args$cex.main)) cm <- 1 else cm <- args$cex.main
  if (titleOnly)  title(zlab,cex.main=cm) else
  { xpos<-xrange[1]+3*xr/10
  xl<-c(xpos,xpos+xr/10); yl<-c(ypos,ypos)
  lines(xl,yl,xpd=TRUE,lwd=args$lwd)
  text(xpos+xr/10,ypos,zlab,xpd=TRUE,pos=4,cex=cs*cm,off=0.5*cs*cm)
  }
  if  (is.null(args$cex.axis)) cma <- 1 else cma <- args$cex.axis
  axis(1,cex.axis=cs*cma);axis(2,cex.axis=cs*cma);box();
  if  (is.null(args$cex.lab)) cma <- 1 else cma <- args$cex.lab
  mtext(xlab,1,2.5,cex=cs*cma);mtext(ylab,2,2.5,cex=cs*cma)
  if (!"lwd"%in%n.args) args$lwd<-1
  if (!"lty"%in%n.args) args$lty<-2
  if (!"col"%in%n.args) args$col<-2
  if (!"labcex"%in%n.args) args$labcex<-cs*.5
  zz <- trans(z+zse+shift)
  args$z<-substitute(zz)
  
  do.call("contour",args)
  
  if (!titleOnly) {
    xpos<-xrange[1]
    xl<-c(xpos,xpos+xr/10)#;yl<-c(ypos,ypos)
    lines(xl,yl,xpd=TRUE,lty=args$lty,col=args$col)
    text(xpos+xr/10,ypos,paste("-",round(se.mult),"se",sep=""),xpd=TRUE,pos=4,cex=cs*cm,off=0.5*cs*cm)
  }
  
  if (!"lty"%in%n.args) args$lty<-3
  if (!"col"%in%n.args) args$col<-3
  zz <- trans(z - zse+shift)
  args$z<-substitute(zz)
  do.call("contour",args)
  
  if (!titleOnly) {
    xpos<-xrange[2]-xr/5
    xl<-c(xpos,xpos+xr/10);
    lines(xl,yl,xpd=TRUE,lty=args$lty,col=args$col)
    text(xpos+xr/10,ypos,paste("+",round(se.mult),"se",sep=""),xpd=TRUE,pos=4,cex=cs*cm,off=0.5*cs*cm)
  }
}