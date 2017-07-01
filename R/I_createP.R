.createP <- function(sm, x, partial.resids, 
                     rug, se, scale, n, n2,
                     pers, theta, phi, jit, xlab, ylab, main, label,
                     ylim, xlim, too.far, shade, shade.col,
                     se1.mult, se2.mult, shift, trans,
                     by.resids, scheme, seWithMean, fitSmooth, w.resid,
                     resDen, ...)
{
  first <- sm$first.para
  last <- sm$last.para
  edf <- sum(x$edf[first:last]) ## Effective DoF for this term
  term.lab <- .subEDF(sm$label, edf)
  attr(sm, "coefficients") <- x$coefficients[first:last] # Relevant coeffs for i-th smooth
  P <- .prepare(sm, data=x$model, partial.resids=partial.resids,
                rug=rug, se=se, scale=scale, n=n, n2=n2,
                pers=pers,theta=theta,phi=phi,jit=jit,xlab=xlab,ylab=ylab,main=main,label=term.lab,
                ylim=ylim,xlim=xlim,too.far=too.far,shade=shade,shade.col=shade.col,
                se1.mult=se1.mult,se2.mult=se2.mult,shift=shift,trans=trans,
                by.resids=by.resids,scheme=scheme,...)
  
  if (is.null(P)){
    P <- list(plot.me=FALSE) 
  } else {
    if (is.null(P$fit)) { 
      p <- x$coefficients[first:last]   ## relevant coefficients 
      offset <- attr(P$X,"offset")      ## any term specific offset
      ## get fitted values ....
      if (is.null(offset)) P$fit <- P$X%*%p else P$fit <- P$X%*%p + offset 
      if (!is.null(P$exclude)) P$fit[P$exclude] <- NA
      if (se && P$se) { ## get standard errors for fit
        ## test whether mean variability to be added to variability (only for centred terms)
        if (seWithMean && attr(sm,"nCons")>0) {
          if (length(x$cmX) < ncol(x$Vp)) x$cmX <- c(x$cmX,rep(0,ncol(x$Vp)-length(x$cmX)))
          X1 <- matrix(x$cmX,nrow(P$X),ncol(x$Vp),byrow=TRUE)
          meanL1 <- sm$meanL1
          if (!is.null(meanL1)) X1 <- X1 / meanL1
          X1[,first:last] <- P$X
          se.fit <- sqrt(pmax(0,rowSums((X1%*%x$Vp)*X1)))
        } else se.fit <- ## se in centred (or anyway unconstained) space only
            sqrt(pmax(0,rowSums((P$X%*%x$Vp[first:last,first:last,drop=FALSE])*P$X)))
        if (!is.null(P$exclude)) P$se.fit[P$exclude] <- NA
      } ## standard errors for fit completed
      if (partial.resids || (resDen!="none")) { P$p.resid <- fitSmooth + w.resid }
      if (se && P$se) P$se <- se.fit*P$se.mult  # Note multiplier
      P$X <- NULL
    } else { ## P$fit created directly
      if (partial.resids || (resDen!="none")) { P$p.resid <- fitSmooth + w.resid }
    }
    P$plot.me <- TRUE
  }
  return( list("P" = P, "coef" = attr(sm, "coefficients")) ) 
}