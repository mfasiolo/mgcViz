#' @noRd
.prepare.fs.interaction <- function(x, data = NULL, label = "",
                                    n = 100, xlab = NULL, ylab = NULL, main = NULL,
                                    ylim = NULL, xlim = NULL, ...) {
  if (x$dim > 1){
    stop("no method for base smooth dim > 1")
  }
  raw <- data[x$base$term][[1]]

  # Generate x sequence for prediction
  if (is.null(xlim)){ xlim <- range(raw) }
  xx <- seq(xlim[1], xlim[2], length = n) 

  nf <- length(x$flev)
  fac <- rep(x$flev, rep(n, nf))
  dat <- data.frame(fac, xx)
  names(dat) <- c(x$fterm, x$base$term)
  X <- PredictMat(x, dat)
  xlabel <- if (is.null(xlab)) x$base$term else xlab
  ylabel <- if (is.null(ylab)) label else ylab
  return(list(
    X = X, scale = TRUE, se = FALSE, raw = raw, xlim = xlim, ylim = ylim,
    xlab = xlabel, ylab = ylabel, main = main, x = xx, n = n, nf = nf
  ))
} 