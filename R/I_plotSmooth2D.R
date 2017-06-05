.plotSmooth2D <- function(x, P=NULL, data=NULL, label="", se1.mult=1, se2.mult=2,
                          partial.resids=FALSE, rug=TRUE, se=TRUE, scale=FALSE, n=100, n2=40,
                          pers=FALSE, theta=30, phi=30, jit=FALSE, xlab=NULL, ylab=NULL, main=NULL,
                          ylim=NULL, xlim=NULL, too.far=0.1, shade=FALSE, shade.col="gray80",
                          shift=0, trans=I, by.resids=FALSE, scheme=0, hcolors=heat.colors(50),
                          contour.col=3, inter = FALSE, ...)
{
  if(se){
    P$fit[P$exclude] <- NA
    if (pers) scheme <- 1
    if (scheme == 1) { ## perspective plot 
      persp(P$x,P$y,matrix(trans(P$fit+shift),n2,n2),xlab=P$xlab,ylab=P$ylab,
            zlab=P$main,ylim=P$ylim,xlim=P$xlim,theta=theta,phi=phi,...)
    } else if (scheme==2||scheme==3) {
      if (scheme==3) hcolors <- grey(0:50/50)
      image(P$x,P$y,matrix(trans(P$fit+shift),n2,n2),xlab=P$xlab,ylab=P$ylab,
            main=P$main,xlim=P$xlim,ylim=P$ylim,col=hcolors,...)
      contour(P$x,P$y,matrix(trans(P$fit+shift),n2,n2),add=TRUE,col=contour.col,...)
      if (rug) {  
        if (is.null(list(...)[["pch"]])) points(P$raw$x,P$raw$y,pch=".",...) else
          points(P$raw$x,P$raw$y,...)
      }
    } else { ## contour plot with error contours
      sp.contour(P$x,P$y,matrix(P$fit,n2,n2),matrix(P$se,n2,n2),
                 xlab=P$xlab,ylab=P$ylab,zlab=P$main,titleOnly=!is.null(main),
                 se.mult=1,trans=trans,shift=shift,...)
      if (rug) { 
        if (is.null(list(...)[["pch"]]))
          points(P$raw$x,P$raw$y,pch=".",...) else
            points(P$raw$x,P$raw$y,...) 
      }
    } ## counter plot done 
  } else {
    P$fit[P$exclude] <- NA
    if (!is.null(main)) P$title <- main
    if (pers) scheme <- 1
    if (scheme==1) { 
      persp(P$x,P$y,matrix(trans(P$fit+shift),n2,n2),xlab=P$xlab,ylab=P$ylab,
            zlab=P$main,theta=theta,phi=phi,xlim=P$xlim,ylim=P$ylim,...)
    } else if (scheme==2||scheme==3) {
      if (scheme==3) hcolors <- grey(0:50/50)
      image(P$x,P$y,matrix(trans(P$fit+shift),n2,n2),xlab=P$xlab,ylab=P$ylab,
            main=P$main,xlim=P$xlim,ylim=P$ylim,col=hcolors,...)
      contour(P$x,P$y,matrix(trans(P$fit+shift),n2,n2),add=TRUE,col=contour.col,...)
      if (rug) {  
        if (is.null(list(...)[["pch"]])) points(P$raw$x,P$raw$y,pch=".",...) else
          points(P$raw$x,P$raw$y,...)
      }
    } else { 
      contour(P$x,P$y,matrix(trans(P$fit+shift),n2,n2),xlab=P$xlab,ylab=P$ylab,
              main=P$main,xlim=P$xlim,ylim=P$ylim,...)
      if (rug) {  
        if (is.null(list(...)[["pch"]])) points(P$raw$x,P$raw$y,pch=".",...) else
          points(P$raw$x,P$raw$y,...)
      }
    }
  }
}