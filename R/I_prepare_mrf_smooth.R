#' @noRd
#' @export
.prepare.mrf.smooth <- function(x, data, label, se1.mult, se2.mult,
                                partial.resids, se, n, n2,
                                xlab, ylab, main,
                                ylim, xlim, too.far,
                                trans, phi, theta, scheme, ...) {
  
  raw <- data[x$term][[1]]
  dat <- data.frame(x=factor(names(x$xt$polys),levels=levels(x$knots)))
  names(dat) <- x$term
  X <- PredictMat(x,dat)   # prediction matrix for this term
  if (is.null(xlab)) xlabel<- "" else xlabel <- xlab
  if (is.null(ylab)) ylabel <- "" else ylabel <- ylab
  return(list(X=X, scale=FALSE, se=FALSE, raw=raw, xlab=xlabel, ylab=ylabel,
              main=label))
  
}