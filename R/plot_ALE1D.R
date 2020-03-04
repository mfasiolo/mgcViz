#'
#' Plot 1D Accumulated Local Effects (ALE) 
#'
#' @param x a 1D ALE effects, produced by the \code{ALE} function
#' @param trans monotonic function to apply to the ALE effect, before plotting.
#'              Monotonicity is not checked. 
#' @param maxpo maximum number of rug lines that will be used by \code{l_rug}. 
#'              If number of datapoints > \code{maxpo}, then a subsample of \code{maxpo} points will be taken.
#' @param nsim number of ALE effect curves to be simulated from the posterior distribution. 
#'             These can be plotted using the \link{l_simLine} layer. See Examples section below. 
#' @param ... currently not used.
#' @return An objects of class \code{plotSmooth}.
#' @references Apley, D.W., and Zhu, J, 2016. Visualizing the effects of predictor variables in black 
#'             box supervised learning models. arXiv preprint arXiv:1612.08468.
#' @author Matteo Fasiolo and Christian Capezza, with some internal code having been adapted from the ALEPlot 
#'         package of Dan Apley.
#' @examples 
#' 
#' library(mgcViz)
#' # Here x1 and x2 are very correlated, but only 
#' # x1 has influence of the response
#' set.seed(4141)
#' n <- 1000
#' X <- rmvn(n, c(0, 0), matrix(c(1, 0.9, 0.9, 1), 2, 2))
#' y <- X[ , 1] + 0.2 * X[ , 1]^2 + rnorm(n, 0, 0.8)
#' dat <- data.frame(y = y, x1 = X[ , 1], x2 = X[ , 2])
#' fit <- gam(y ~ te(x1, x2), data = dat)
#' 
#' # Marginal plot suggests that E(y) depends on x2, but
#' # this is due to the correlation between x1 and x2...
#' plot(dat$x2, fit$fitted.values)
#' 
#' # ... in fact ALE effect of x2 is flat ...
#' plot(ALE(fit, "x2")) + l_ciPoly() + l_fitLine() + l_rug()
#' 
#' # ... while ALE effect of x1 is strong.
#' plot(ALE(fit, "x1", center = 2), nsim = 20) + 
#'   l_simLine() + l_fitLine()
#'   
#' @name plot.ALE1D
#' @rdname plot.ALE1D
#' @export plot.ALE1D
#' @export
#' 
plot.ALE1D <- function(x, trans = identity, maxpo = 1e4, nsim = 0, ...) {
  
  .dat <- list()
  .dat$fit <- x$ALE$ALE
  .dat$fit$ty <- trans(x$ALE$ALE$y)
  .dat$misc <- list("trans" = trans, "varALE" = x$ALE$varALE)
  .dat$res <- data.frame("x" = x$xval)
  
  # Sample if too many points (> maxpo)  
  nres <- nrow( .dat$res )
  .dat$res$sub <- if(nres > maxpo) { 
    sample( c(rep(TRUE, maxpo), rep(FALSE, nres-maxpo)) )
  } else { 
    rep(TRUE, nres) 
  }
  
  if( nsim > 0 ){
    simFx <- t( rmvn(nsim, .dat$fit$y, x$ALE$varALE) )
    
    .dat$sim <- data.frame("x" = rep(.dat$fit$x, nsim), 
                           "ty" = trans(as.vector(simFx)), 
                           "id" = as.factor(rep(1:nsim, each = nrow(simFx))), 
                           stringsAsFactors = TRUE)
  }
  
  .pl <- ggplot(data = .dat$fit, aes("x" = x, "y" = ty)) +
    labs(title = NULL, x = x$xnam, y = paste("f(", x$xnam, ")", sep = '')) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
  
  if( x$type == "factor" ){
    .pl <- .pl +  scale_x_discrete()
  }
  
  return( structure(list("ggObj" = .pl, "data" = .dat, "type" = c("ALE1D", .simpleCap(x$type))), 
                    class = c("plotSmooth",  "gg")) )
  
}

