#' Plotting random effects
#' 
#' @description Plotting random effects.
#' @name plot.random.effect
#' @rdname plot.random.effect
#' @export plot.random.effect
#' @examples 
#' library(mgcv)
#' b <- gam(travel~s(Rail,bs="re"), data=Rail, method="REML")
#' b <- getViz(b)
#' plot(sm(b, 1)) + l_fitLine(colour = 2, linetype = 2) + l_points() + 
#'   l_ciLine(colour = 4, linetype = 3)
#' 
#' plot(sm(b, 1)) + l_ciPoly() + l_points()
#' 
#' # Default
#' plot(b)
#'  
plot.random.effect <- function(x, trans = identity, ...) {
  
  # 1) Prepare data
  P <- .prepareP(o = x, unconditional = FALSE, residuals = TRUE, 
                 resDen = "none", se = TRUE, se.mult = 1, n = 100, n2 = NULL,  
                 xlab = NULL, ylab = NULL, main = NULL, ylim = NULL, xlim = NULL,
                 too.far = NULL, seWithMean = FALSE)
  
  # 2) Produce output object
  out <- .plot.random.effect(x = P$smooth, P = P, trans = trans)
  
  class(out) <- c("plotSmooth", "randomEffect", "gg")
  
  return(out)
  
}


########################
#' @noRd
.plot.random.effect <- function(x, P, trans){
  
  .dat <- list()
  
  .n <- length(P$fit)
  .dat$fit <- data.frame(x = qnorm(ppoints(.n)), 
                         y = sort(P$fit), 
                         ty = trans(sort(P$fit)))

  .dat$misc <- list("trans" = trans)  
  
  .pl <- ggplot(data = .dat$fit, mapping = aes(x = x, y = ty)) +
                labs(title = P$main, x = P$xlab, y = P$ylab) + 
                theme_bw() +
                theme(panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank())
  
  return( list("ggObj" = .pl, "data" = .dat) )
  
}

