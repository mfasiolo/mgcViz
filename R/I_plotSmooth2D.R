.plotSmooth2D <- function(x, P=NULL, data=NULL, label="", se1.mult=1, se2.mult=2,
                          partial.resids=FALSE, rug=TRUE, se=TRUE, scale=FALSE, n=100, n2=40,
                          pers=FALSE, theta=30, phi=30, jit=FALSE, xlab=NULL, ylab=NULL, main=NULL,
                          ylim=NULL, xlim=NULL, too.far=0.1, shade=FALSE, shade.col="gray80",
                          shift=0, trans=I, by.resids=FALSE, scheme=0, hcolors=viridis(50),
                          contour.col=1, inter = FALSE, ...)
{
  if(se){
    P$fit[P$exclude] <- NA
    if (pers) scheme <- 1
    if (scheme == 1) { ## perspective plot 
      persp(P$x,P$y,matrix(trans(P$fit+shift),n2,n2),xlab=P$xlab,ylab=P$ylab,
            zlab=P$main,ylim=P$ylim,xlim=P$xlim,theta=theta,phi=phi,...)
    } else if (scheme==2||scheme==3) {
      if (scheme==3) hcolors <- grey(0:50/50)
      .pl <- ggplot(data = data.frame("z"=P$fit, "x"=rep(P$x, length(P$fit)/length(P$x)), 
                                      "y"=rep(P$y, each=length(P$fit)/length(P$x)), p=pnorm( abs(P$fit)/(P$se/se2.mult))^3+0.1), 
                    aes(x=x, y=y, z=z)) + geom_raster(aes(fill = z, alpha = p)) + geom_contour(color=contour.col, na.rm=T) + 
                    scale_fill_gradientn(colours = hcolors,na.value="grey") + scale_alpha(guide = 'none') +
                    coord_cartesian(xlim=P$xlim, ylim=P$ylim, expand=F) + labs(title = P$main, x = P$xlab, y = P$ylab)
      # Add partial residuals
      if (rug) { 
          .tmpF <- function(..., shape = ifelse(inter, 'a', '.'), col = ifelse(inter, "grey", "black")) # Alter default shape and col
          {
            geom_point(data=data.frame("resx"=P$raw$x, "resy"=P$raw$y), aes(x = resx, y = resy), 
                       inherit.aes = FALSE, shape = shape, col = col, ...)
          } 
          .pl <- .pl + .tmpF(...)
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
  
  return( .pl )
  
}