
# Internal function for preparing plot of two dimensional smooths
.preparePlotSmoothMD <- function(x, fix, data = NULL, se.mult = 2, n2 = 40, label = "",
                                 xlab = NULL, ylab = NULL, main = NULL,
                                 ylim = NULL, xlim = NULL, too.far = 0.1,  ...) {
  out <- NULL
  if (x$plot.me) {
    ov <- names( fix )
    iv <- x$term[ !(x$term %in% ov) ]
    xterm <- iv[1]
    yterm <- iv[2]
    xlabel <- ifelse(is.null(xlab), xterm, xlab)
    ylabel <- ifelse(is.null(ylab), yterm, ylab)
    raw <- data.frame(x = as.numeric(data[xterm][[1]]),
                      y = as.numeric(data[yterm][[1]]))
    n2 <- max(10, n2)
    if (is.null(xlim)) {
      xm <- seq(min(raw$x), max(raw$x), length = n2)
    } else { 
      xm <- seq(xlim[1], xlim[2], length = n2)
    }
    if (is.null(ylim)) {
      ym <- seq(min(raw$y), max(raw$y), length = n2)
    } else {
      ym <- seq(ylim[1], ylim[2], length = n2)
    }
    xx <- rep(xm, n2)
    yy <- rep(ym, each = n2)
    
    # Mark cells on X-Y grid are too far from any observation  
    if (too.far[1] > 0) { 
      exclude <- exclude.too.far(xx, yy, raw$x, raw$y, dist = too.far[1])
    } else {
      exclude <- rep(FALSE, n2 * n2)
    }
    
    # Mark covariate vectors (and corresponding residuals) that are too 
    # far from X-Y plane (the slice of interest)
    if (is.na(too.far[2]) || too.far[2] > 0) {
      tmp <- sapply(ov, function(.nm) as.numeric(data[.nm][[1]])) 
      tmp <- sqrt(maha(tmp, fix, diag(diag(cov(tmp)), ncol(tmp)))) # Euclidean distance
      exclude2 <- tmp > if( is.na(too.far[2]) ){ quantile(tmp, 0.1) } else { too.far[2] } 
    } else { 
      exclude2 <- FALSE
    }
    if (x$by != "NA") {        # deal with any by variables
      by <- rep(1, n2^2)
      dat <- data.frame(x = xx, y = yy, by = by)
      colnames(dat) <- c(xterm, yterm, x$by)
    } else { 
      dat <- data.frame(x = xx, y = yy)
      colnames(dat) <- c(xterm, yterm)
    }  ## prediction data.frame complete
    
    for(ii in ov){ dat[[ii]] <- rep(fix[ii], n2^2) }
    
    X <- PredictMat(x, dat)   ## prediction matrix for this term
    if (is.null(main)) {
      # TODO: are label/main both necessary ? seems not
      main <- label
    }
    if (is.null(ylim)) {
      ylim <- range(ym) 
    }
    if (is.null(xlim)) {
      xlim <- range(xm) 
    }
    out <- list(X = X, x = xm, y = ym, scale = FALSE, se = TRUE,
                raw = raw, xlab = xlabel, ylab = ylabel,
                main = main, se.mult = se.mult, ylim = ylim,
                xlim = xlim, exclude = exclude, exclude2 = exclude2)
  }
  return(out)
}