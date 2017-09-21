
.fastKernDens <- function(dat, bw, ngr, xlimit, ylimit, tol, cond, ...) {
  
  if( is.null(xlimit) ) { xlimit <- range(dat[ , 1]) }
  if( is.null(ylimit) ) { ylimit <- range(dat[ , 2]) }
  
  # Suppress warnings related to ngrid being too small relative to bw. Happens with big dataset.
  withCallingHandlers({
    
    # Get bandwidth
    if( is.null(bw) ) {
      bw <- c(dpik(dat[ , 1], range.x = xlimit, gridsize = ngr[1]), 
              dpik(dat[ , 2], range.x = ylimit, gridsize = ngr[2]))
    }
    
    # Estimate joint density
    dXY <- bkde2D(dat, range.x = list(xlimit, ylimit), gridsize = ngr, bandwidth = bw)
    
    # Estimate conditional density
    dX <- NULL
    if( cond ) { 
      # Calculate conditional density of residuals | x
      tmp <- 1e-8 / sqrt(2*pi*var(dat[ , 1])) # Small constant, to avoid dividing by almost zero
      dX <- bkde(dat[ , 1], gridsize = ngr[1], range.x = xlimit, bandwidth = bw[1])
      dXY$fhat <- dXY$fhat / ( dX$y + tmp )
    } 
    
  }, warning = function(w) invokeRestart("muffleWarning") )
  
  # Set NA points where p(y|x) is too low
  dXY$fhat[ dXY$fhat <= tol / sqrt(2*pi*var(dat[ , 2])) ] <- NA 
  
  return( list( "dXY" = dXY, "dX" = dX) )
}