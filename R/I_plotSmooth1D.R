
# Internal function for plotting one dimensional smooths
.plotSmooth1D <- function(x, P, partial.resids=FALSE, rug=TRUE, se=TRUE, scale=TRUE, n=100,
                           jit=FALSE, shade=FALSE, shade.col="gray80", ylim = NULL,
                           shift=0, trans=I, by.resids=FALSE, scheme=0, inter = FALSE, ...)
{
  if (scheme == 1){ shade <- TRUE }
  
  ul <- P$fit + P$se ## upper CL
  ll <- P$fit - P$se ## lower CL  
  
  if (scale==FALSE && is.null(ylim)) { # Calculate ylim of plot
    ylimit <- range(c(if(partial.resids){ P$p.resid } else { P$fit }, if(se){ c(ul, ll) } else { c() }), na.rm = TRUE) 
  }
  
  ylimit <- if (is.null(ylim)){ ylimit <- ylimit + shift } else { ylim }
  
  .pl <- ggplot(data=data.frame(x=P$x, y=trans(P$fit+shift), uci=trans(ul+shift), lci=trans(ll-shift)), aes(x=x, y=y)) + 
    xlim(P$xlim[1], P$xlim[2]) + ylim(trans(ylimit[1]), trans(ylimit[2])) + labs(title = P$main, x = P$xlab, y = P$ylab)
  
  # Add partial residuals
  if (partial.resids&&(by.resids||x$by=="NA")) { 
    if (length(P$raw)==length(P$p.resid)) {
      .tmpF <- function(..., shape = ifelse(inter, 'a', '.'), col = ifelse(inter, "grey", "black")) # Alter default shape and col
      {
        geom_point(data = data.frame(resx = P$raw, resy = trans(P$p.resid+shift)), aes(x = resx, y = resy),
                   shape = shape, col = col, ...)
      }
      .pl <- .pl + .tmpF(...)
    } else {
      warning("Partial residuals do not have a natural x-axis location for linear functional terms")
    }
  }
  
  # Add shade or lines for confidence bands
  if(se){
    if( shade ){ 
      polygon(c(P$x,P$x[n:1],P$x[1]),
              trans(c(ul,ll[n:1],ul[1])+shift), col = shade.col,border = NA)
    } else {
      .tmpF <- function(pl, ..., linetype="dashed") # Alter default "linetype"
      {
        pl <- pl + geom_line(aes(x, uci), linetype=linetype, ...) + geom_line(aes(x, lci), linetype=linetype, ...)
      }
      .pl <- .tmpF(.pl, ...)
    }
  }
  
  # Add rug
  if (rug) { 
    .df <- data.frame(x = if(jit){ jitter(as.numeric(P$raw)) } else { as.numeric(P$raw) } )
    .tmpF <- function(pl, ..., size = 0.2) # Alter default "size"
    {
      geom_rug(data = .df, aes(x = x), inherit.aes = F, size = size, ...)
    }
    .pl <- .pl + .tmpF(.pl, ...)
  } 
  
  .pl <- .pl + geom_line(...) + theme_bw() 
  
  return( .pl )
} 
