##########
# Internal method
##########
.prepareInnerNested <- function(o,
                                n,
                                xlim,
                                ylim = NULL,
                                smooth = FALSE,
                                ...) {
  if (!exists("expsmooth") || !exists("mgks")) {
    expsmooth <- mgks <- function(x) {
      
    }
    stop("Please install the gamFactory package.")
  }
  
  gObj <- o$gObj
  sm <- gObj$smooth[[o$ism]]
  
  si <- sm$xt$si
  alpha <- si$alpha
  B <- si$B
  
  da <- length(alpha)
  prange <- (sm$first.para:sm$last.para)[1:da]
  type <- class(o)[1]
  
  # Remove scale parameter of inner transformation
  if (type %in% c("mgks", "nexpsm")) {
    prange <- prange[-1]
    da <- da - 1
    alpha <- alpha[-1]
    if (is.null(B)) {
      B <- diag(1, nrow = length(alpha))
    }
  }
  
  Va <- gObj$Vp[prange, prange, drop = FALSE]
  
  if (type == "si_nexpsm") {
    if (!smooth) {
      # coef plot
      alpha_center <- si$alpha_center
      alpha_si <- si$alpha_si
      alpha_nexp <- si$alpha_nexp
      n_si <- si$n_si
      n_nexp <- si$n_nexp
      
      if (is.null(alpha_center)) {
        alpha_center <- alpha_si * 0
      }
      
      alpha_si <- drop(si$B_si %*% (alpha_si + alpha_center))
      alpha_nexp <- drop(si$B_nexp %*% alpha_nexp)
      alpha <- c(alpha_nexp, alpha_si)
      
      Va_nexp <- si$B_nexp %*% Va[1:n_nexp, 1:n_nexp, drop = FALSE] %*% t(si$B_nexp)
      Va_si <- si$B_si %*% Va[(n_nexp + 1):(n_nexp + n_si), (n_nexp + 1):(n_nexp + n_si), drop = FALSE] %*% t(si$B_si)
      se_si <- sqrt(pmax(0, diag(Va_si)))
      se_nexp <- sqrt(pmax(0, diag(Va_nexp)))
      se <- c(se_nexp, se_si)
      edf   <- sum(gObj$edf[prange])
      ylabel <- .subEDF(paste0("Inner_coef(", sm$term, ")"), edf)
      xlabel <- "Index (NEXP + SI)"
      
      out <- list(
        fit = unname(alpha),
        x = 1:da,
        se = se,
        xlab = xlabel,
        ylab = ylabel,
        main = NULL,
        type = "si_nexpsm_coef"
      )
    } else {
      # smooth eff plot
      xa <- sm$xt$xa #values after smooth
      times <- 1:length(xa)
      edf   <- sum(gObj$edf[prange])
      ylabel <- .subEDF(paste0("expsm(", sm$term, ")"), edf)
      xlabel <- "Index"
      
      out <- list(
        fit = xa,
        x = times,
        xlab = xlabel,
        ylab = ylabel,
        main = NULL,
        type = "si_nexpsm_xa"
      )
    }
  }
  
  if (type == "nexpsm") {
    inner <- expsmooth(
      y = si$x,
      Xi = si$X,
      beta = alpha,
      deriv = 1
    )
    fit <- inner$d0
    Jac <- inner$d1
    if (!is.null(si$times)) {
      fit <- fit[1:max(si$times)]
      Jac <- Jac[1:max(si$times), ]
    }
    nobs <- length(fit)
    se <- sqrt(pmax(0, rowSums((Jac %*% Va) * Jac)))
    edf   <- sum(gObj$edf[prange])
    ylabel <- .subEDF(paste0("expsm(", sm$term, ")"), edf)
    xlabel <- "Index"
    
    if (!is.null(xlim)) {
      xlim <- sort(xlim)
      xlim[1] <- max(xlim[1], 1)
      xlim[2] <- min(xlim[2], nobs)
      ii <- which(1:nobs >= xlim[1] & 1:nobs <= xlim[2])
      nobs <- length(ii)
    } else {
      xlim <- c(1, nobs)
      ii <- 1:nobs
    }
    
    out <- list(
      "fit" = fit[ii],
      "x" = ii,
      "se" = se[ii],
      "p.resid" = si$x[ii],
      "raw" = ii,
      "xlim" = xlim,
      xlab = xlabel,
      ylab = ylabel,
      main = NULL,
      type = "nexpsm"
    )
    
  }
  
  if (type == "si") {
    a0 <- si$a0
    if (is.null(a0)) {
      a0 <- alpha * 0
    }
    alpha <- drop(B %*% (alpha + a0))
    Va <- B %*% Va %*% t(B)
    se <- sqrt(pmax(0, diag(Va)))
    edf   <- sum(gObj$edf[prange])
    ylabel <- .subEDF(paste0("Inner_coef(", sm$term, ")"), edf)
    xlabel <- "Index"
    
    out <- list(
      "fit" = alpha,
      "x" = 1:da,
      "se" = se,
      xlab = xlabel,
      ylab = ylabel,
      main = NULL,
      type = "si"
    )
    
  }
  
  # NOT CLEAR HOW TO DO THIS WITH DISTANCEs
  # if( type == "mgks" ){
  #   d <- ncol(si$X0)
  #   if( d != 2 ){ return( NULL ) }
  #   # ONLY 2D case handled at the moment!!
  #
  #   if( !is.null(xlim) ) xlim <- sort(xlim) else xlim <- range(si$X[ , 1])
  #   if( !is.null(ylim) ) ylim <- sort(ylim) else ylim <- range(si$X[ , 2])
  #
  #   xx <- rep(seq(xlim[1], xlim[2], length.out = n), n)
  #   yy <- rep(seq(ylim[1], ylim[2], length.out = n), rep(n, n))
  #   X <- cbind(xx, yy)
  #
  #   si$x <- as.matrix(si$x)
  #   if( ncol(si$x) > 1 ){ si$x <- colMeans(si$x) }
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