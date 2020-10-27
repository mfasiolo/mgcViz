
##########
# Internal method
#
.prepareInnerSI <- function(o, n, xlim, ...){
  
  gObj <- o$gObj
  sm <- gObj$smooth[[ o$ism ]]
  
  # Get single index vector
  si <- sm$xt$si
  dsi <- length( si$alpha )
  
  # Get regression coeff of outer smooth
  prange <- (sm$first.para:sm$last.para)[1:dsi]

  fit <- si$B %*% si$alpha
  
  xx <- 1:length(fit)
  
  se <- sqrt(pmax(0, rowSums((si$B %*% gObj$Vp[prange, prange, drop = FALSE]) * si$B)))
  
  edf   <- sum(gObj$edf[prange])
  ylabel <- .subEDF(paste0("coef(", sm$term, ")"), edf)
  xlabel <- "Index"
  out <- list("fit" = fit, "x" = xx, "se" = se,
              xlab = xlabel, ylab = ylabel, main = NULL)
  return(out)
  
}