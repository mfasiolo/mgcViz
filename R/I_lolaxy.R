.lolaxy <- function(lo,la,theta,phi) {
  ## takes locations lo,la, relative to a pole at lo=theta, la=phi. 
  ## theta, phi are expressed relative to plotting co-ordinate system 
  ## with pole at top. Convert to x,y in plotting co-ordinates.
  ## all in radians!
  er <- .repole(-lo,la,-pi,phi)
  er$lo <- er$lo - theta
  y <- sin(er$la)
  x <- cos(er$la)*sin(er$lo)
  z <- cos(er$la)*cos(er$lo)
  ind <- z<0
  list(x=x[ind],y=y[ind])
} ## end of lolaxy