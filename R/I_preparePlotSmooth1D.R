
# Internal function for preparing plot of one dimensional smooths
.preparePlotSmooth1D <- function(x, data, label = "", se.mult = 1,
                                 n = 100, xlim = NULL, xlab = NULL,
                                 ylab = NULL, main = NULL, ...) {
  out <- NULL
  if (x$plot.me) {
    raw <- as.vector( data[x$term][[1]] )
    if (is.null(xlim)){ # Generate x sequence for prediction
      xx <- seq(min(raw), max(raw), length = n)
      } else {
        xx <- seq(xlim[1], xlim[2], length = n) 
      }
    if (x$by != "NA"){  # Deal with any by variables
      by <- rep(1, n)
      dat <- data.frame(x = xx, by = by)
      names(dat) <- c(x$term, x$by)
    } else { 
      dat <- data.frame(x = xx)
      names(dat) <- x$term
    } # Finished preparing prediction data.frame
    X <- PredictMat(x, dat)   # prediction matrix for this term
    xlabel <- ifelse(is.null(xlab), x$term, xlab)
    ylabel <- ifelse(is.null(ylab), label, ylab)
    if (is.null(xlim)) {
      xlim <- range(xx) 
    }
    out <- list(X = X, x = xx, scale = TRUE, se = TRUE,
                raw = raw, xlab = xlabel, ylab = ylabel, 
                main = main, se.mult = se.mult, xlim = xlim)
  }
  return(out)
}