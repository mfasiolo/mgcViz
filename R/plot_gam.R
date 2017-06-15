#' Basic GAM plotting
#' 
#' @description XXX
#' @importFrom plotly ggplotly
#' @import viridis
#' @name plot.gam
#' @examples 
#' library(mgcViz)
#' @rdname plot.gam
#' @export plot.gam
plot.gam <- function(x,residuals=FALSE,rug=TRUE,se=TRUE,pages=0,select=NULL,scale=T,n=100,n2=40,
                     pers=FALSE,theta=30,phi=30,jit=FALSE,xlab=NULL,ylab=NULL,main=NULL,
                     ylim=NULL,xlim=NULL,too.far=0.1,all.terms=FALSE,shade=FALSE,shade.col="gray80",
                     shift=0,trans=I,seWithMean=FALSE,unconditional=FALSE,by.resids=FALSE,scheme=0,
                     draw=TRUE, inter=FALSE, ...)
{ 
  m <- length(x$smooth) # number of smooth effects
  
  if (length(scheme)==1) scheme <- rep(scheme, m)
  if (length(scheme)!=m) { 
    warning( paste("scheme should be a single number, or a vector with", m, "elements") )
    scheme <- rep(scheme[1], m)
  }
  
  # This creates/modifies variables in the environment.
  # INPUTS: unconditional, x, residuals, se, fitSmooth
  # NEW/MODIFIED VARIABLES: x, w.resid, partial.resids, se2.mult, se1.mult, se, fv.terms, order  
  fv.terms <- NULL
  eval( .initializeXXX )
  
  # Loop to get the data for the plots
  pd <- list(); # List of data to be plotted
  ii <- 1 # needs a value if no smooths is present, but parametric terms are...
  if (m>0){ 
    for (ii in 1:m) { ## work through smooth terms
      tmp <- .createP(sm=x$smooth[[ii]], x=x, partial.resids=partial.resids,
                      rug=rug, se=se, scale=scale, n=n, n2=n2,
                      pers=pers, theta=theta, phi=phi, jit=jit, xlab=xlab, ylab=ylab, main=main, label=term.lab,
                      ylim=ylim, xlim=xlim, too.far=too.far, shade=shade, shade.col=shade.col,
                      se1.mult=se1.mult, se2.mult=se2.mult, shift=shift, trans=trans,
                      by.resids=by.resids, scheme=scheme[ii], seWithMean=seWithMean, 
                      fitSmooth=fv.terms[ , length(order)+ii],
                      w.resid=w.resid, inter=inter, ...)
      pd[[ii]] <- tmp[["P"]]
      attr(x$smooth[[ii]], "coefficients") <- tmp[["coef"]]
      rm(tmp)
    }
  }
  
  # Plot parametric terms as well?
  if (all.terms){ n.para <- sum(order==1) } else { n.para <- 0 } 
  
  ##############################################
  ## sort out number of pages and plots per page 
  ##############################################
  
  n.plots <- n.para
  if (m>0) for (i in 1:m) n.plots <- n.plots + as.numeric(pd[[i]]$plot.me)
  
  if (n.plots==0) stop("No terms to plot - nothing for plot.gam() to do.")
  
  if (pages>n.plots) pages<-n.plots
  if (pages<0) pages<-0
  if (pages!=0)    # figure out how to display things
  { ppp<-n.plots%/%pages
  if (n.plots%%pages!=0)
  { ppp<-ppp+1
  while (ppp*(pages-1)>=n.plots) pages<-pages-1
  }
  
  # now figure out number of rows and columns
  c <- r <- trunc(sqrt(ppp))
  if (c<1) r <- c <- 1
  if (c*r < ppp) c <- c + 1
  if (c*r < ppp) r <- r + 1
  oldpar<-par(mfrow=c(r,c))
  
  } else
  { ppp<-1;oldpar<-par()}
  
  if ((pages==0&&prod(par("mfcol"))<n.plots&&dev.interactive())||
      pages>1&&dev.interactive()) ask <- TRUE else ask <- FALSE
  
  if (!is.null(select)) {
    ask <- FALSE
  }
  
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  
  #####################################
  ## get a common scale, if required...
  #####################################
  if (scale==TRUE && is.null(ylim)) {
    k <- 0
    if (m>0) for (i in 1:m) if (pd[[i]]$plot.me&&pd[[i]]$scale) { ## loop through plot data 
      if (se&&length(pd[[i]]$se)>1) { ## require CIs on plots
        ul<-pd[[i]]$fit+pd[[i]]$se
        ll<-pd[[i]]$fit-pd[[i]]$se
        if (k==0) { 
          ylim <- c(min(ll,na.rm=TRUE),max(ul,na.rm=TRUE));k <- 1
        } else {
          if (min(ll,na.rm=TRUE)<ylim[1]) ylim[1] <- min(ll,na.rm=TRUE)
          if (max(ul,na.rm=TRUE)>ylim[2]) ylim[2] <- max(ul,na.rm=TRUE)
        }
      } else { ## no standard errors
        if (k==0) {
          ylim <- range(pd[[i]]$fit,na.rm=TRUE);k <- 1
        } else {
          if (min(pd[[i]]$fit,na.rm=TRUE)<ylim[1]) ylim[1] <- min(pd[[i]]$fit,na.rm=TRUE)
          if (max(pd[[i]]$fit,na.rm=TRUE)>ylim[2]) ylim[2] <- max(pd[[i]]$fit,na.rm=TRUE)
        }
      }
      if (partial.resids) { 
        ul <- max(pd[[i]]$p.resid,na.rm=TRUE)
        if (ul > ylim[2]) ylim[2] <- ul
        ll <-  min(pd[[i]]$p.resid,na.rm=TRUE)
        if (ll < ylim[1]) ylim[1] <- ll
      } ## partial resids done
    } ## loop end 
    ylim <- trans(ylim+shift)
  } ## end of common scale computation
  
  ##############################################################
  ## now plot smooths, by calling plot methods with plot data...
  ##############################################################
  
  .ggobj <- list()
  if (m>0) for (i in 1:m) if (pd[[i]]$plot.me&&(is.null(select)||i==select)) {
    withCallingHandlers({
      .ggobj[[i]] <- .plot(x$smooth[[i]],P=pd[[i]],partial.resids=partial.resids,rug=rug,se=se,
                           scale=scale,n=n,n2=n2,pers=pers,theta=theta,phi=phi,jit=jit,xlab=xlab,
                           ylab=ylab,main=main,ylim=ylim,xlim=xlim,too.far=too.far,shade=shade,
                           shade.col=shade.col,shift=shift,trans=trans,by.resids=by.resids,scheme=scheme[i], 
                           inter=inter, ...)
      if(draw){ if(inter){print(ggplotly(.ggobj[[i]]+theme_bw()))}else{print(.ggobj[[i]]+theme_bw())} }
    }, warning = function(w) {
      if (length(grep("Ignoring unknown parameters: ", conditionMessage(w))))
      {
        invokeRestart("muffleWarning")
      }
    })
  }
  
  ####################################################
  ## Finally deal with any parametric term plotting...
  ####################################################
  
  if (n.para>0) # plot parameteric terms
  { class(x) <- c("gam","glm","lm") # needed to get termplot to call model.frame.glm 
  if (is.null(select)) {
    attr(x,"para.only") <- TRUE
    termplot(x,se=se,rug=rug,col.se=1,col.term=1,main=attr(x$pterms,"term.labels"),...)
  } else { # figure out which plot is required
    if (select > m) { 
      ## can't figure out how to get this to work with more than first linear predictor
      ## as termplots relies on matching terms to names in original data... 
      select <- select - m # i.e. which parametric term
      term.labels <- attr(x$pterms,"term.labels")
      term.labels <- term.labels[order==1]
      if (select <= length(term.labels)) {
        # if (interactive() && m &&i%%ppp==0) 
        termplot(x,terms=term.labels[select],se=se,rug=rug,col.se=1,col.term=1,...)
      }  
    }
  }
  }
  if (pages>0) par(oldpar)
  attr(.ggobj, "rawData") <- pd
  invisible(.ggobj)
} ## end plot.gam
