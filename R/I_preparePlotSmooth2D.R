
# Internal function for preparing plot of two dimensional smooths
.preparePlotSmooth2D <- function(x, data = NULL, se.mult = 2, n2 = 40, label = "",
                                 xlab = NULL, ylab = NULL, main = NULL,
                                 ylim = NULL, xlim = NULL, too.far = 0.1,  ...) {
  out <- NULL
  if (x$plot.me) {
    xterm <- x$term[1]
    yterm <- x$term[2]
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
    yy <- rep(ym, rep(n2, n2))
    if (too.far > 0) {
      exclude <- exclude.too.far(xx, yy, raw$x, raw$y, dist = too.far)
    } else {
      exclude <- rep(FALSE, n2 * n2)
    }
    if (x$by != "NA") {        # deal with any by variables
      by <- rep(1, n2^2)
      dat <- data.frame(x = xx, y = yy, by = by)
      colnames(dat) <- c(xterm, yterm, x$by)
    } else { 
      dat <- data.frame(x = xx, y = yy)
      colnames(dat) <- c(xterm, yterm)
    }  ## prediction data.frame complete
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
                       xlim = xlim, exclude = exclude)
  }
  return(out)
}