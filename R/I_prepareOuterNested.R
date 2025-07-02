
##########
# Internal method that prepare non-linear effects plots (only single index at the moment)
#
.prepareOuterNested <- function(o, n, xlim, ...){
  
  if(!exists("expsmooth") || !exists("mgks") ){
    expsmooth <- mgks <- function(x){}
    stop("Please install the gamFactory package.")
  }
  
  gObj <- o$gObj
  sm <- gObj$smooth[[ o$ism ]]
  si <- sm$xt$si
  alpha <- si$alpha
  a0 <- si$a0
  dsi <- length( alpha )
  
  rescale <- function(x){ exp(alpha[1]) * (x - si$xm) }
  
  type <- class(o)[1]
  if( type == "si" ){
   raw <- sort( si$X %*% (alpha + a0) )
   rescale <- function(x) x # No rescaling needed!
   trnam <- "proj"
  } 
  if( type == "nexpsm" ){
   raw <- expsmooth(y = si$x, Xi = si$X, beta = alpha[-1], times = si$times)$d0
   trnam <- "expsm"
  }
  if( type == "mgks" ){
    raw <- mgks(y = si$x, dist = si$dist, beta = alpha[-1])$d0
    trnam <- "mgks"
  }

  # Get regression coeff of outer smooth
  prange <- (sm$first.para:sm$last.para)[-(1:dsi)]
  beta <- coef( gObj )[ prange ]
  
  # Generate x sequence for prediction
  if (is.null(xlim)){ 
    xlim <- range(raw)
  } 
  xx <- seq(xlim[1], xlim[2], length = n) 
  
  # Compute outer model matrix
  X <- sm$xt$basis$evalX(x = rescale(xx), deriv = 0)$X0
  
  fit <- X %*% beta
  
  se <- sqrt(pmax(0, rowSums((X %*% gObj$Vp[prange, prange, drop = FALSE]) * X)))
  
  edf   <- sum(gObj$edf[prange])
  ylabel <- .subEDF(paste0("s(",trnam,"(", sm$term, "))"), edf)
  xlabel <- paste0(trnam,"(", sm$term, ")")
  out <- list("fit" = fit, "x" = xx, "se" = se, "raw" = raw, "xlim" = xlim,
              xlab = xlabel, ylab = ylabel, main = NULL)
  return(out)
  
}