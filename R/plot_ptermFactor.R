#'
#' Plotting factor or logical parametric effects
#' 
#' @description These are the plotting methods for parametric factor or logical effects.
#' @name plot.ptermFactor
#' @param x a factor or logical parametric effect object, extracted using [mgcViz::pterm].
#' @param maxpo maximum number of residuals points that will be used by layers such as
#'              \code{resRug()} and \code{resPoints()}. If number of datapoints > \code{maxpo},
#'              then a subsample of \code{maxpo} points will be taken.
#' @param trans monotonic function to apply to the fit, confidence intervals and residuals, 
#'              before plotting. Monotonicity is not checked. 
#' @param a.facet arguments to be passed to [ggplot2::facet_wrap] or [ggplot2::facet_grid]. The former gets
#'                called when \code{fix} contains one vector, the latter when \code{fix} contains two vectors.
#' @param asFact relevant only when working with models fitted with \link{mqgamV}. If
#'               \code{FALSE} quantile of interest (qu) is treated as a continuous variable, otherwise as 
#'               a factor.
#' @param ... currently unused.
#' @return An object of class \code{plotSmooth}.
#' @examples 
#' # Simulate data and fit GAM
#' set.seed(3)
#' dat <- gamSim(1,n=2000,dist="normal",scale=20)
#' dat$fac <- as.factor( sample(c("A1", "A2", "A3"), nrow(dat), replace = TRUE) )
#' dat$logi <- as.logical( sample(c(TRUE, FALSE), nrow(dat), replace = TRUE) )
#' bs <- "cr"; k <- 12
#' b <- gam(y~fac + s(x0) + s(x1) + s(x2) + s(x3) + logi, data=dat)
#' o <- getViz(b, nsim = 0)
#' 
#' # Extract factor terms and plot it
#' pt <- pterm(o, 1)
#' plot(pt) + l_ciBar() + l_fitPoints(colour = 2) + l_rug(alpha = 0.2)
#' 
#' # Use barplot instead of points
#' pt <- pterm(o, 1)
#' plot(pt) + l_fitBar() + l_ciBar()
#' 
#' # Same with binary varible
#' pt <- pterm(o, 2)
#' plot(pt) + l_fitPoints() + l_ciBar()
#' 
#' @rdname plot.ptermFactor
#' @export plot.ptermFactor
#' @export
#' 
plot.ptermFactor <- function(x, maxpo = 1e4, trans = identity, ...){
  
  if(x$order > 1){ 
    message("mgcViz does not know how to plot this effect. Returning NULL.")
    return( invisible(NULL) ) 
  }
  
  gObj <- x$gObj
  
  # 1) Do prediction
  X <- gObj$model
  
  vr <- as.factor( X[[ x$varNam ]] )
  xx <- as.factor( levels(vr) )
  data <- X[1:length(xx), ]
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
  .dat$misc <- list("trans" = trans)
  
  # 3) Get partial residuals
  .dat$res <- data.frame("x" = as.factor(gObj$model[[x$varNam]]))
  
  # Check if partial residuals are defined: for instance the are not for gamlss models
  if (is.null(gObj$residuals) || is.null(gObj$weights)) {
    .dat$res$y <- NULL
  } else {
    .wr <- sqrt(gObj$weights)
    .wr <- gObj$residuals * .wr / mean(.wr)  # weighted working residuals
    .dat$res$y <- trans( .wr + 
                           gObj$store$termsFit[ , which(colnames(gObj$store$termsFit) == x$nam)] )
  }
  
  # Sample if too many points (> maxpo)  
  nres <- nrow( .dat$res )
  .dat$res$sub <- if(nres > maxpo) { 
    sample( c(rep(T, maxpo), rep(F, nres-maxpo)) )
  } else { 
    rep(T, nres) 
  }
  
  .pl <- ggplot(data = .dat$fit, aes("x" = x, "y" = ty)) +
    labs(title = NULL, x = x$nam, y = paste("f(", x$nam, ")", sep = '')) + 
    scale_x_discrete() +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
  
  return( structure(list("ggObj" = .pl, "data" = .dat, "type" = c("Pterm", "Factor")), 
                    class = c("plotSmooth",  "gg")) )
  
}

