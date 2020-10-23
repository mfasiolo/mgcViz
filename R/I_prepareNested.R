
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
  
  if (is.null(xlim)){ # Generate x sequence for prediction
    xx <- seq(min(raw), max(raw), length = n)
  } else {
    xx <- seq(xlim[1], xlim[2], length = n) 
  }
  
  # Compute outer model matrix
  X <- sm$xt$splineDes(x = xx, deriv = 0)$X0
  
  fit <- X %*% beta
  
  se <- sqrt(pmax(0, rowSums((X %*% gObj$Vp[prange, prange, drop = FALSE]) * X)))
  
  xlabel <- paste0(sm$term, " %*% alpha")
  ylabel <- paste0("s(", sm$term, " %*% alpha)")
  out <- list("fit" = fit, "x" = xx, "se" = se, xlab = xlabel, ylab = ylabel, main = NULL)
  return(out)
  
}