
##########
# Internal method
#
.prepareInnerNested <- function(o, n, xlim, ...){
  
  gObj <- o$gObj
  sm <- gObj$smooth[[ o$ism ]]
  
  # Get single index vector
  si <- sm$xt$si
  dsi <- length( si$alpha )
  prange <- (sm$first.para:sm$last.para)[1:dsi]
  
  type <- class(o)[1]
  if(type == "si"){
    # Get parameters of inner transformation
    
    
    fit <- si$B %*% si$alpha
    
    xx <- 1:length(fit)
    
    se <- sqrt(pmax(0, rowSums((si$B %*% gObj$Vp[prange, prange, drop = FALSE]) * si$B)))
    
    edf   <- sum(gObj$edf[prange])
    ylabel <- .subEDF(paste0("proj_coef(", sm$term, ")"), edf)
    xlabel <- "Index"
    out <- list("fit" = fit, "x" = xx, "se" = se,
                xlab = xlabel, ylab = ylabel, main = NULL)
    return(out)
  }
  if( type == "nexpsm" ){
    raw <- exp(alpha[1]) * (expsmooth(y = si$x, Xi = si$X, beta = alpha[-1])$d0 - si$xm)
    trnam <- "expsm"
  }
  
}