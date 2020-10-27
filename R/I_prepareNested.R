
##########
# Internal method that prepare non-linear effects plots (only single index at the moment)
#
.prepareNested<- function(o, n, xlim, ...){
  
  gObj <- o$gObj
  sm <- gObj$smooth[[ o$ism ]]
  
  # Get single index vector
  si <- sm$xt$si
  raw <- sort( si$X %*% si$alpha )
  dsi <- length( si$alpha )
  
  # Get regression coeff of outer smooth
  prange <- (sm$first.para:sm$last.para)[-(1:dsi)]
  beta <- coef( gObj )[ prange ]
  
  # Generate x sequence for prediction
  if (is.null(xlim)){ 
    xlim <- range(raw)
  } 
  xx <- seq(xlim[1], xlim[2], length = n) 
  
  # Compute outer model matrix
  X <- sm$xt$splineDes(x = xx, deriv = 0)$X0
  
  fit <- X %*% beta
  
  se <- sqrt(pmax(0, rowSums((X %*% gObj$Vp[prange, prange, drop = FALSE]) * X)))
  
  edf   <- sum(gObj$edf[prange])
  ylabel <- .subEDF(paste0("s(proj(", sm$term, "))"), edf)
  xlabel <- paste0("proj(", sm$term, ")")
  out <- list("fit" = fit, "x" = xx, "se" = se, "raw" = raw, "xlim" = xlim,
              xlab = xlabel, ylab = ylabel, main = NULL)
  return(out)
  
}