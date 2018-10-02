#'
#' Plotting numeric parametric effects
#' 
#' @description This is the plotting method for parametric numerical effects.
#' @name plot.ptermNumeric
#' @param x a numerical parametric effect object, extracted using [mgcViz::pterm].
#' @param n number of grid points used to compute main effect and c.i. lines. 
#' @param xlim if supplied then this pair of numbers are used as the x limits for the plot.
#' @param maxpo maximum number of residuals points that will be used by layers such as
#'              \code{resRug()} and \code{resPoints()}. If number of datapoints > \code{maxpo},
#'              then a subsample of \code{maxpo} points will be taken.
#' @param trans monotonic function to apply to the fit, confidence intervals and residuals, 
#'              before plotting. Monotonicity is not checked. 
#' @param ... currently unused.
#' @return An object of class \code{plotSmooth}.
#' @examples 
#' # Simulate data and fit GAM
#' set.seed(3)
#' dat <- gamSim(1,n=2000,dist="normal",scale=20)
#' bs <- "cr"; k <- 12
#' b <- gam(y ~  x0 + x1 + I(x1^2) + s(x2,bs=bs,k=k) + 
#'               I(x1*x2) + s(x3, bs=bs), data=dat)
#' o <- getViz(b, nsim = 0)
#' 
#' # Extract first terms and plot it
#' pt <- pterm(o, 1)
#' plot(pt, n = 60) + l_ciPoly() + l_fitLine() + l_ciLine()
#' 
#' # Extract I(x1^2) terms and plot it with partial residuals
#' pt <- pterm(o, 3)
#' plot(pt, n = 60) + l_ciPoly() + l_fitLine() + l_ciLine() + l_points()
#'
#' @importFrom mgcv predict.gam
#' @rdname plot.ptermNumeric
#' @export plot.ptermNumeric
#' @export
#' 
plot.ptermNumeric <- function(x, n = 100, xlim = NULL, maxpo = 1e4, trans = identity, ...){
  
  if(x$order > 1){ 
    message("mgcViz does not know how to plot this effect. Returning NULL.")
    return( invisible(NULL) ) 
  }
  
  gObj <- x$gObj
  
  # 1) Do prediction
  X <- gObj$model
  
  if(n > nrow(X)){ # Model matrix too short, we make it longer
    X <- X[rep(1:nrow(X), ceiling(n/nrow(X))), ]
  }
  
  if( is.null(xlim) ){ xlim <- range(X[[x$varNam]]) }
  
  xx <- seq(xlim[1], xlim[2], length = n)
  data <- X[1:n, ]
  data[[x$varNam]] <- xx
  
  # Suppressing spurious warnings from predict.gam
  .pred <- withCallingHandlers(predict.gam(gObj, type = "terms", se.fit = TRUE, terms = x$nam, newdata = data), 
                               warning = function(w){ 
                                 if(is.list(gObj$formula) && any(grepl("is absent, its contrast will be ignored", w))){ 
                                   invokeRestart( "muffleWarning" )
                                 }
                               })
  
  # 2) Build dataset on fitted effect
  .dat <- list()
  .dat$fit <- data.frame("x"  = xx,
                         "y"  = unname(.pred$fit),
                         "ty"  = trans( unname(.pred$fit) ),
                         "se" = unname(.pred$se) )
  
  # 3) Get partial residuals
  .dat$res <- data.frame("x" = as.vector(gObj$model[[x$varNam]]))
  
  # Check if partial residuals are defined: for instance the are not for gamlss models
  if (is.null(gObj$residuals) || is.null(gObj$weights)) {
    .dat$res$y <- NULL
  } else {
    .wr <- sqrt(gObj$weights)
    .wr <- gObj$residuals * .wr / mean(.wr)  # weighted working residuals
    .dat$res$y <- trans( .wr + gObj$store$termsFit[ , which(colnames(gObj$store$termsFit) == x$nam)] )
  }
  
  # Exclude residuals falling outside boundaries
  .dat$res <- .dat$res[.dat$res$x >= xlim[1] & .dat$res$x <= xlim[2], , drop = FALSE]
  
  # Sample if too many points (> maxpo)  
  nres <- nrow( .dat$res )
  .dat$res$sub <- if(nres > maxpo) { 
    sample( c(rep(T, maxpo), rep(F, nres-maxpo)) )
  } else { 
    rep(T, nres) 
  }
  
  # 4) Build output plot
  .dat$misc <- list("trans" = trans)
  
  .pl <- ggplot(data = .dat$fit, aes("x" = x, "y" = ty)) +
    labs(title = NULL, x = x$nam, y = paste("f(", x$nam, ")", sep = '')) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  return( structure(list("ggObj" = .pl, "data" = .dat, "type" = c("Pterm", "Numeric")), 
                    class = c("plotSmooth", "gg")) )
  
}
