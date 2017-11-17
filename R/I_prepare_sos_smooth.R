.prepare.sos.smooth <- function(x, data, label, se1.mult = 1, se2.mult = 1,
                                partial.resids = NULL, se, n, n2,
                                xlab = NULL, ylab = NULL, main = NULL,
                                ylim = NULL, xlim = NULL, too.far,
                                trans, phi, theta, scheme) {
  
  ## plot method function for sos.smooth terms
  if (scheme == 1){ 
    return(.prepare.mgcv.smooth(x = x, data = data, label = label, se1.mult = se1.mult, 
                                se2.mult = se2.mult, n = n, n2 = n2,  xlab = xlab, ylab = ylab, 
                                main = main, ylim = ylim, xlim = xlim, 
                                too.far = too.far))
  }
  
  ## convert location of pole in plotting grid to radians
  phi <- phi*pi/180
  theta <- theta*pi/180
  
  ## re-map to sensible values...
  theta <- theta%%(2*pi)
  if (theta>pi) theta <- theta - 2*pi
  
  phi <- phi%%(2*pi)
  if (phi > pi) phi <- phi - 2*pi
  if (phi > pi/2) phi <- pi - phi
  if (phi < -pi/2 ) phi <- -(phi+pi)  
  
  if (!x$plot.me) return(NULL) ## shouldn't or can't plot
  ## get basic plot data 
  raw <- data[ x$term ]
  raw <- as.data.frame( .lolaxy(lo=raw[[2]]*pi/180, la=raw[[1]]*pi/180, theta, phi) )

  m <- round(n2*1.5)
  ym <- xm <- seq(-1,1,length=m)
  gr <- expand.grid(x=xm,y=ym)
  r <- z <- gr$x^2+gr$y^2
  z[z>1] <- NA
  z <- sqrt(1-z)
  
  ## generate la, lo in plotting grid co-ordinates...
  ind <- !is.na(z) 
  r <- r[ind]
  la <- asin(gr$y[ind])
  lo <- cos(la)
  lo <- asin(gr$x[ind]/lo)
  
  um <- .repole(lo,la,theta,phi)
  
  dat <- data.frame(la=um$la*180/pi,lo=um$lo*180/pi)
  names(dat) <- x$term
  if (x$by!="NA") dat[[x$by]] <- la*0+1    
  
  X <- PredictMat(x,dat)   # prediction matrix for this term
  
  ## fix lo for smooth contouring
  lo <- dat[[2]]
  ii <- lo <= -177
  lo[ii] <- lo[ii] <- 360 + lo[ii]
  ii <- lo < -165 & lo > -177 
  ii <- ii | (abs(dat[[1]])>80) 
  lo[ii] <- NA
  
  return(list(X=X,scale=FALSE,se=TRUE,raw=raw,xlab="",ylab="",main="",
              ind=ind,xm=xm,ym=ym,lo=lo,la=dat[[1]],se.mult = se1.mult))
  
} ## end prepare.sos.smooth