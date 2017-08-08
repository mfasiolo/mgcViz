
# Internal function, used by check.mgcv.smooth.2D and check.mgcv.smooth.MD

.check.mgcv.smooth.2D <- function(o, type, binw1, binw2, 
                                  gridFun, nco, xlimit, ylimit, 
                                  palette1, palette2, acFun, too.far=0, fix=NULL)
{
  type <- match.arg(type, c("auto", "deviance", "pearson", "scaled.pearson", 
                            "working", "response", "tunif", "tnormal"))
  
  # Returns the appropriate residual type for each GAM family
  if( type=="auto" ) { type <- .getResTypeAndMethod(o$gObj$family$family)$type }
  
  if( is.null(gridFun) ){
    gridFun <- function(.x, .sdr){
      .o <- mean(.x) / { .sdr/sqrt(length(.x)) }
      return( .o )
    }
  }
  
  #### 1) Preparation
  o$smooth <- o$gObj$smooth[[o$ism]]
  
  # Initialization
  fv.terms <- o$store$termsFit[ , o$store$np + o$ism]
  init <- .initializeXXX(o = o, unconditional = FALSE, residuals = FALSE, resDen = FALSE, se = FALSE, 
                         fv.terms = o$store$termsFit[ , o$store$np + o$ism])

  # Prepare for plotting
  P <- .createP(sm=init$o$smooth, x=init$o$gObj, partial.resids=init$partial.resids,
                se=FALSE, n=NULL, n2=nco, xlab=NULL, ylab=NULL, main=NULL, 
                ylim=ylimit, xlim=xlimit, too.far=too.far, se1.mult=init$se1.mult, se2.mult=init$se2.mult, 
                seWithMean=NULL, fitSmooth=init$fv.terms, w.resid=init$w.resid, fix=fix, resDen=FALSE)[["P"]] 
                #, ...) # ... not needed here, I guess
  
  # Get residuals or transformed responses
  ty <- residuals(init$o$gObj, type = type) 
  X <- data.frame("x"=rep(P$x, nco), "y"=rep(P$y, each=nco))
  X$fit <- P$fit
  
  # Drop data too far from current slice (relevant when called from check.mgcv.smooth.MD)
  if( any(P$exclude2) ){
   ty <- ty[ !P$exclude2 ]
   P$raw <- P$raw[ !P$exclude2, ]
  }
  
  # Drop data outside limits
  sdat <- filter(data.frame("x"=P$raw$x, "y"=P$raw$y, "z"=ty), 
                 findInterval(x, P$xlim)==1 & findInterval(y, P$ylim)==1) 
  
  # Determine bin widths for the two plots
  if( is.null(binw1) ){  binw1 <- c(diff(range(sdat$x))/20,  diff(range(sdat$y))/20)  }
  if( is.null(binw2) ){  binw2 <- c(diff(range(P$raw$x))/20,  diff(range(P$raw$y))/20) }
  
  # Compute acf
  acfO <- .acfOptim(dat = sdat, fn = acFun)
  
  # Compute lines in the direction of the optimized acf
  smD <- data.frame("x"=rep(seq(min(sdat$x), max(sdat$x), length.out = 10), 10), 
                    "y"=rep(seq(min(sdat$y), max(sdat$y), length.out = 10), each=10))
  smD$zx <- acfO$fn[[1]](smD$x, smD$y)
  smD$zy <- acfO$fn[[2]](smD$x, smD$y)

  #### 2) Plotting
  .pl <- list()
  
  # Plot with hexagons
  .pl1 <- ggplot(data = sdat, aes(x=x, y=y, z=z)) + 
    stat_summary_hex(binwidth = binw1, 
                     fun = function(.x, .sdr){ gridFun(na.omit(.x), .sdr) }, 
                     fun.args = list(".sdr" = sqrt(init$o$gObj$sig2))) +
    scale_fill_gradientn(colours = palette1, na.value="white") +
    coord_cartesian(xlim=NULL, ylim=NULL, expand=F) +
    geom_contour(data=X, aes(x=x, y=y, z=fit), color="black", na.rm=T, inherit.aes = F) + 
    labs(title = paste(P$main, if(!is.null(P$exclude2)){paste(", n =", nrow(P$raw))}else{''}, sep=''), 
         x = P$xlab, y = P$ylab)
  
  # ACF plots
  .pl2 <- ggplot(data = acfO$df[[1]], mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_hline(aes(yintercept = acfO$ci[1]), color = "blue", linetype="dashed") +
    geom_hline(aes(yintercept = -acfO$ci[1]), color = "blue", linetype="dashed") +
    geom_segment(mapping = aes(xend = lag, yend = 0))
  
  .pl3 <- ggplot(data = acfO$df[[2]], mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_hline(aes(yintercept = acfO$ci[2]), color = "red", linetype="dashed") +
    geom_hline(aes(yintercept = -acfO$ci[2]), color = "red", linetype="dashed") +
    geom_segment(mapping = aes(xend = lag, yend = 0)) + coord_flip()
  
  # Plot of observations, using hexagon
  .pl4 <- ggplot(P$raw, aes(x=x, y=y)) + 
    geom_hex(binwidth = binw2) + 
    geom_contour(data=smD, aes(x=x, y=y, z=zx), color="blue", inherit.aes = F, alpha = 0.3) +
    geom_contour(data=smD, aes(x=x, y=y, z=zy), color="red", inherit.aes = F, alpha = 0.3) +
    scale_fill_gradientn(colours = palette2) + 
    geom_polygon(data=data.frame("x"=P$xlim[c(1, 1, 2, 2)], "y"=P$ylim[c(1, 2, 2, 1)]), 
                 colour="black", fill=NA) + 
    labs(x = P$xlab, y = P$ylab) + 
    guides(fill=FALSE) #+ scale_alpha_identity()
  
  .pl <- list("pl1"=.pl1, "pl2"=.pl2, "pl3"=.pl3, "pl4"=.pl4)
  .pl <- lapply(.pl, function(.inp) .inp+theme_bw())
  
  return( .pl )
}
