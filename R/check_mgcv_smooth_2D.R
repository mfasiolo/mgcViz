#' Checking two dimensional smooth effects
#' 
#' @description XXX
#' @name check.mgcv.smooth.2D
#' @examples 
#' library(mgcViz);
#' 
#' # Simulate data from Rosenbrock function
#' n <- 1e3
#' X <- data.frame("x1"=rnorm(n, 0.5, 0.5), "x2"=rnorm(n, 1.5, 1))
#' X$y <- (1-X$x1)^2 + 100*(X$x2 - X$x1^2)^2 + rnorm(n, 0, 2)
#' b <- gam(y ~ te(x1, x2, k = 5), data = X, method = "REML")#, discrete = T)
#' 
#' v <- getViz(b)
#' o <- v(1) 
#' 
#' # Check residuals: k is too low to model the effect of x1 and x2 correctly,  
#' # hence the residuals are far from iid.
#' a<-check(o, xlim=c(-1, 1), ylim=c(0, 3))
#' 
#' a # calls print.check.smooth.2D
#' @rdname check.mgcv.smooth.2D
#' @importFrom dplyr filter sample_n
#' @export check.mgcv.smooth.2D
check.mgcv.smooth.2D <- function(o, type="deviance", binw1=NULL, binw2=NULL, 
                                 gridFun=NULL, nco=40, xlimit=NULL, ylimit=NULL, 
                                 palette1=viridis(50, begin=0.2), 
                                 palette2=rev(gray.colors(20)), 
                                 acFun=list(NULL, NULL))
{
  if( !("mgcv.smooth.2D" %in% class(o)) ) { stop("\"o\" should be of class \"mgcv.smooth.2D\"") }
  
  type <- match.arg(type, c("deviance", "pearson", "scaled.pearson", 
                            "working", "response", "tunif", "tnormal"))
  
  if( is.null(gridFun) ){
    gridFun <- function(.x, .sdr){
      .o <- pnorm(mean(.x), 0, .sdr/sqrt(length(.x)))
      return( .o )
    }
  }
  
  # Choose what kind of residuals or transformed responses to use
  if( type %in% c("tunif", "tnormal") ){
    fam <- fix.family.cdf( o$gObj$family )
    ty <- fam$cdf(o$gObj$y, o$gObj$fitted.values, o$gObj$prior.weights, o$gObj$sig2, logp = TRUE)
    if( type == "tnormal" ) { ty <- qnorm(ty, log.p = TRUE) } else { ty <- exp(ty) } 
  } else {
    ty <- residuals(o, type = type) 
  }
  
  o$smooth <- o$gObj$smooth[[o$ism]]
  
  # This creates/modifies variables in the environment.
  # INPUTS: unconditional, o, residuals, se
  # NEW/MODIFIED VARIABLES: o, w.resid, partial.resids, se2.mult, se1.mult, se, fv.terms, order  
  fv.terms <- o$store$termsFit[ , o$store$np + o$ism]
  init <- .initializeXXX(o = o, unconditional = FALSE, residuals = FALSE, resDen = FALSE, se = FALSE, 
                         fv.terms = o$store$termsFit[ , o$store$np + o$ism])
  # affect initialize output
  o <- init$o
  w.resid <- init$w.resid
  partial.resids <- init$partial.resids
  se2.mult <- init$se2.mult
  se1.mult <- init$se1.mult
  se <- init$se
  fv.terms <- init$fv.terms
  order <- init$order
  
  # Prepare for plotting
  P <- .createP(sm=o$smooth, x=o$gObj, partial.resids=partial.resids,
                se=FALSE, n=NULL, n2=nco, xlab=NULL, ylab=NULL, main=NULL, 
                ylim=ylimit, xlim=xlimit, too.far=0, se1.mult=NULL, se2.mult=NULL, 
                seWithMean=NULL, fitSmooth=fv.terms, w.resid=w.resid, resDen=FALSE)[["P"]] #, ...) # ... not needed here, I guess
  
  X <- data.frame("x"=rep(P$x, nco), "y"=rep(P$y, each=nco))
  X$fit <- P$fit
  
  sdat <- filter(data.frame("x"=P$raw$x, "y"=P$raw$y, "z"=ty), 
                 findInterval(x, P$xlim)==1 & findInterval(y, P$ylim)==1) 
  
  if( is.null(acFun[[1]]) || is.null(acFun[[2]]) ){
    # Rotation that maximizes absolute value of acf
    tmpS <- if( nrow(sdat)<1e5 ){ sdat } else { sample_n(sdat, 1e5) }  
    thSeq <- seq(0, pi/2, length.out=10)
    fval <- sapply(thSeq, 
                   function(.in){
                     sum(abs(acf(tmpS$z[order(cos(.in)*tmpS$x+sin(.in)*tmpS$y)], plot=F)$acf[-1]))
                   })
    theta <- thSeq[which.max(fval)]
    
    tmpCreator <- function(.th) function(.x, .y) cos(.th)*.x+sin(.th)*.y
    if( is.null(acFun[[1]]) ){
      acFun[[1]] <- tmpCreator(theta)
      if( is.null(acFun[[2]]) ){ acFun[[2]] <- tmpCreator(theta+pi/2) }
    } else {
      if( is.null(acFun[[2]]) ){ acFun[[2]] <- tmpCreator(theta) }
    }
  }
  
  if(is.null(binw1)){
    binw1 <- c(diff(range(sdat$x))/20,  diff(range(sdat$y))/20)
  }
  
  .pl <- list()
  .pl1 <- ggplot(data = sdat, aes(x=x, y=y, z=z)) + 
    stat_summary_hex(binwidth = binw1, fun = gridFun, fun.args = list(".sdr" = sqrt(o$gObj$sig2))) +
    scale_fill_gradientn(colours = palette1, na.value="white") +
    coord_cartesian(xlim=NULL, ylim=NULL, expand=F) +
    geom_contour(data=X, aes(x=x, y=y, z=fit), color="black", na.rm=T, inherit.aes = F)
  
  bacf <- acf(sdat$z[order(acFun[[1]](sdat$x, sdat$y))], plot = FALSE)
  ci0 <- qnorm((1 + 0.95)/2)/sqrt(bacf$n.used)
  bacfdf <- with(bacf, data.frame(lag, acf))
  bacfdf <- bacfdf[-1, ]
  
  .pl2 <- ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_hline(aes(yintercept = ci0), color = "blue", linetype="dashed") +
    geom_hline(aes(yintercept = -ci0), color = "blue", linetype="dashed") +
    geom_segment(mapping = aes(xend = lag, yend = 0))
  
  bacf <- acf(sdat$z[order(acFun[[2]](sdat$x, sdat$y))], plot = FALSE)
  ci0 <- qnorm((1 + 0.95)/2)/sqrt(bacf$n.used)
  bacfdf <- with(bacf, data.frame(lag, acf))
  bacfdf <- bacfdf[-1, ]
  
  .pl3 <- ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_hline(aes(yintercept = ci0), color = "red", linetype="dashed") +
    geom_hline(aes(yintercept = -ci0), color = "red", linetype="dashed") +
    geom_segment(mapping = aes(xend = lag, yend = 0)) + coord_flip()
  
  smD <- data.frame("x"=rep(seq(min(P$raw$x), max(P$raw$x), length.out = 10), 10), 
                    "y"=rep(seq(min(P$raw$y), max(P$raw$y), length.out = 10), each=10))
  smD$zx <- acFun[[1]](smD$x, smD$y)
  smD$zy <- acFun[[2]](smD$x, smD$y)
  if(is.null(binw2)){ binw2 <- c(diff(range(P$raw$x))/20,  diff(range(P$raw$y))/20) }
  
  .pl4 <- ggplot(P$raw, aes(x=x, y=y)) + 
    geom_hex(binwidth = binw2) + 
    geom_contour(data=smD, aes(x=x, y=y, z=zx), color="blue", inherit.aes = F, alpha = 0.3) +
    geom_contour(data=smD, aes(x=x, y=y, z=zy), color="red", inherit.aes = F, alpha = 0.3) +
    scale_fill_gradientn(colours = palette2) + 
    geom_polygon(data=data.frame("x"=P$xlim[c(1, 1, 2, 2)], "y"=P$ylim[c(1, 2, 2, 1)]), 
                 colour="black", fill=NA) + 
    guides(fill=FALSE) #+ scale_alpha_identity()
  
  .pl <- list("pl1"=.pl1, "pl2"=.pl2, "pl3"=.pl3, "pl4"=.pl4)
  .pl <- lapply(.pl, function(.inp) .inp+theme_bw())
  
  class(.pl) <- "check.smooth.2D"
  
  return( .pl )
}
