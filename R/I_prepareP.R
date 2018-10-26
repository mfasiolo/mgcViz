
# Prepares P list, smooth (sm) and
.prepareP <- function(o, unconditional, residuals, resDen, se, se.mult,
                      n, n2, xlab, ylab, main, ylim, xlim, too.far, 
                      seWithMean, nsim = 0, ...) {
  
  Q <- .initialize(o = o, unconditional = unconditional, residuals = residuals, 
                   se.mult = se.mult, resDen = resDen, se = se)
  
  # Modify Vp here: .createP creates conf int, so we do NOT need to take this into account later
  if( !is.null(Q$Vmat) ) { o$gObj$Vp <- Q$Vmat }
  
  P <- .createP(sm = o$gObj$smooth[[ o$ism ]], 
                x = o$gObj,
                partial.resids = Q$partial.resids,
                se = Q$se, 
                n = n, n2 = n2,
                xlab = xlab, ylab = ylab, 
                main = main, 
                ylim = ylim, xlim = xlim, 
                too.far = too.far, 
                se1.mult = Q$se.mult, se2.mult = Q$se.mult,
                seWithMean = seWithMean, 
                fitSmooth = Q$fv.terms,
                w.resid = Q$w.resid, 
                resDen = resDen, 
                nsim = nsim,
                ...)
  
  P$doPlotResid <- Q$partial.resids

  return( P )
  
}