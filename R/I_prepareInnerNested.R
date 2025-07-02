
##########
# Internal method
#
.prepareInnerNested <- function(o, n, xlim, ylim = NULL, ...){
  
  if(!exists("expsmooth") || !exists("mgks") ){
    expsmooth <- mgks <- function(x){}
    stop("Please install the gamFactory package.")
  }
  
  gObj <- o$gObj
  sm <- gObj$smooth[[ o$ism ]]
  
  si <- sm$xt$si
  alpha <- si$alpha 
  B <- si$B
  
  da <- length( alpha )
  prange <- (sm$first.para:sm$last.para)[1:da]
  type <- class(o)[1]
  
  # Remove scale parameter of inner transformation
  if( type != "si" ){
    prange <- prange[-1]
    da <- da-1
    alpha <- alpha[-1]
    if(is.null(B)){
      B <- diag(1, nrow = length(alpha))
    }
  }
  
  Va <- gObj$Vp[prange, prange, drop = FALSE]
  
  if( type == "nexpsm" ){
    inner <- expsmooth(y = si$x, Xi = si$X, beta = alpha, deriv = 1)
    fit <- inner$d0
    Jac <- inner$d1
    nobs <- length(fit)
    se <- sqrt(pmax(0, rowSums((Jac %*% Va) * Jac)))
    edf   <- sum(gObj$edf[prange])
    ylabel <- .subEDF(paste0("expsm(", sm$term, ")"), edf)
    xlabel <- "Index"
    if( !is.null(xlim) ) {
      xlim <- sort(xlim)
      xlim[1] <- max(xlim[1], 1)
      xlim[2] <- min(xlim[2], nobs)
      ii <- which(1:nobs >= xlim[1] & 1:nobs <= xlim[2])
      nobs <- length(ii)
    } else {
      xlim <- c(1, nobs)
      ii <- 1:nobs
    }
    out <- list("fit" = fit[ii], "x" = ii, "se" = se[ii],
                "p.resid" = si$x[ii], "raw" = ii, 
                "xlim" = xlim, 
                xlab = xlabel, ylab = ylabel, main = NULL, type = "nexpsm")
  } else {
    a0 <- si$a0
    if( is.null(a0) ){
      a0 <- alpha * 0
    }
    alpha <- drop(B %*% (alpha + a0))
    Va <- B %*% Va %*% t(B)
    se <- sqrt(pmax(0, diag(Va)))
    edf   <- sum(gObj$edf[prange])
    ylabel <- .subEDF(paste0("Inner_coef(", sm$term, ")"), edf)
    xlabel <- "Index"
    out <- list("fit" = alpha, "x" = 1:da, "se" = se,
                xlab = xlabel, ylab = ylabel, main = NULL, type = "si")
  }
  # NOT CLEAR HOW TO DO THIS WITH DISTANCEs
  # if( type == "mgks" ){
  #   d <- ncol(si$X0)
  #   if( d != 2 ){ # ONLY 2D case handled at the moment!!
  #     return( NULL )
  #   }
  #   if( !is.null(xlim) ) {
  #     xlim <- sort(xlim)
  #   } else {
  #     xlim <- range(si$X[ , 1])
  #   }
  #   if( !is.null(ylim) ) {
  #     ylim <- sort(ylim)
  #   } else {
  #     ylim <- range(si$X[ , 2])
  #   }
  #   
  #   xx <- rep(seq(xlim[1], xlim[2], length.out = n), n)
  #   yy <- rep(seq(ylim[1], ylim[2], length.out = n), rep(n, n))
  #   
  #   X <- cbind(xx, yy)
  #   si$x <- as.matrix(si$x)
  #   if( ncol(si$x) > 1 ){
  #     si$x <- colMeans(si$x)
  #   }
  #   
  #   inner <- mgks(y = si$x, X = X, X0 = si$X0, beta = alpha[-1], deriv = 1)
  #   fit <- inner$d0
  #   Jac <- inner$d1
  #   se <- sqrt(pmax(0, rowSums((Jac %*% Va[-1, -1, drop = FALSE]) * Jac)))
  #   edf   <- sum(gObj$edf[prange[-1]])
  #   
  #   mainlab <- .subEDF(paste0("mgks(", sm$term, ")"), edf)
  #   ylabel <- "X[ , 2]"
  #   xlabel <- "X[ , 1]"
  #   out <- list("fit" = fit, "X" = si$X, "se" = se, "x" = xx, "y" = yy,
  #               "p.resid" = si$x, "X0" = si$X0, 
  #               "xlim" = xlim, "ylim" = ylim,
  #               "xlab" = xlabel, "ylab" = ylabel, "main" = mainlab, type = "mgks")
  # }
  
  return(out)
  
}