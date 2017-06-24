.repole <- function(lo,la,lop,lap) {
  ## painfully plodding function to get new lo, la relative to pole at
  ## lap,lop...
  ## x,y,z location of pole...
  yp <- sin(lap)
  xp <- cos(lap)*sin(lop)
  zp <- cos(lap)*cos(lop)
  
  ## x,y,z location of meridian point for pole - i.e. point lat pi/2
  ## from pole on pole's lon.
  
  ym <- sin(lap-pi/2)
  xm <- cos(lap-pi/2)*sin(lop)
  zm <- cos(lap-pi/2)*cos(lop)
  
  ## x,y,z locations of points in la, lo
  
  y <- sin(la)
  x <- cos(la)*sin(lo)
  z <- cos(la)*cos(lo)
  
  ## get angle between points and new equatorial plane (i.e. plane orthogonal to pole)
  d <- sqrt((x-xp)^2+(y-yp)^2+(z-zp)^2) ## distance from points to to pole 
  phi <- pi/2-2*asin(d/2)
  
  ## location of images of la,lo on (new) equatorial plane
  ## sin(phi) gives distance to plane, -(xp, yp, zp) is 
  ## direction... 
  x <- x - xp*sin(phi)
  y <- y - yp*sin(phi)
  z <- z - zp*sin(phi)
  
  ## get distances to meridian point
  d <- sqrt((x-xm)^2+(y-ym)^2+(z-zm)^2)
  ## angles to meridian plane (i.e. plane containing origin, meridian point and pole)...
  theta <- (1+cos(phi)^2-d^2)/(2*cos(phi))
  theta[theta < -1] <- -1; theta[theta > 1] <- 1
  theta <- acos(theta)
  
  ## now decide which side of meridian plane...
  
  ## get points at extremes of hemispheres on either side
  ## of meridian plane.... 
  y1 <- 0
  x1 <- sin(lop+pi/2)
  z1 <- cos(lop+pi/2)
  
  y0 <- 0
  x0 <- sin(lop-pi/2)
  z0 <- cos(lop-pi/2)
  
  d1 <- sqrt((x-x1)^2+(y-y1)^2+(z-z1)^2)
  d0 <- sqrt((x-x0)^2+(y-y0)^2+(z-z0)^2)
  
  ii <- d0 < d1 ## index -ve lon hemisphere 
  theta[ii] <- -theta[ii]
  
  list(lo=theta,la=phi)
} ## end of repole
