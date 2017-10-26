#' @noRd
.prepare.random.effect <- function(x, data = NULL, label = "",
                                   n = 100, xlab = NULL, ylab = NULL, main = NULL,
                                   ylim = NULL, xlim = NULL, ...) {
  
  raw <- data[x$term][[1]]
  p <- x$last.para - x$first.para + 1
  X <- diag(p)   # prediction matrix for this term
  if (is.null(xlab)) xlabel<- "Gaussian quantiles" else xlabel <- xlab
  if (is.null(ylab)) ylabel <- "effects" else ylabel <- ylab
  if (!is.null(main)) label <- main
  
  return( list(X=X, scale=FALSE, se=FALSE, 
               raw=raw, xlab=xlabel, ylab=ylabel, main=label) )
}


