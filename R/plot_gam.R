#' Basis GAM plotting
#' 
#' @description XXX
#' @importFrom plotly ggplotly
#' @importFrom viridis viridis
#' @name qqplots
#' @examples 
#' library(mgcViz)
NULL

#' @rdname plot.gam
#' @export
plot.gam <- function(x,residuals=FALSE,rug=TRUE,se=TRUE,pages=0,select=NULL,scale=T,n=100,n2=40,
                     pers=FALSE,theta=30,phi=30,jit=FALSE,xlab=NULL,ylab=NULL,main=NULL,
                     ylim=NULL,xlim=NULL,too.far=0.1,all.terms=FALSE,shade=FALSE,shade.col="gray80",
                     shift=0,trans=I,seWithMean=FALSE,unconditional=FALSE,by.resids=FALSE,scheme=0,
                     draw=TRUE, inter=FALSE, ...)
  
  # Create an appropriate plot for each smooth term of a GAM.....
  # x is a gam object
  # rug determines whether a rug plot should be added to each plot
  # se determines whether twice standard error bars are to be added
  # pages is the number of pages over which to split output - 0 implies that 
  # graphic settings should not be changed for plotting
  # scale T for same y scale for each plot
  #       F for different y scales for each plot
  # n - number of x axis points to use for plotting each term
  # n2 is the square root of the number of grid points to use for contouring
# 2-d terms.

{ 
  ## Local function for producing labels
  sub.edf <- function(lab,edf) {
    ## local function to substitute edf into brackets of label
    ## labels are e.g. smooth[[1]]$label
    pos <- regexpr(":", lab)[1]
    if (pos<0) { ## there is no by variable stuff
      pos <- nchar(lab) - 1
      lab <- paste(substr(lab, start=1, stop=pos),", ", round(edf, digits=2),")",sep="")
    } else {
      lab1 <- substr(lab, start=1, stop=pos-2)
      lab2 <- substr(lab, start=pos-1, stop=nchar(lab))
      lab <- paste(lab1, ",", round(edf,digits=2), lab2, sep="")
    }
    lab
  } ## end of sub.edf
  
  if (unconditional){ # Use Bayesian cov matrix including smoothing parameter uncertainty?
    if (is.null(x$Vc)){ warning("Smoothness uncertainty corrected covariance not available") } 
    else { x$Vp <- x$Vc } 
  }
  
  w.resid <- NULL
  if ( length(residuals)>1 ){ # residuals supplied 
    if (length(residuals)==length(x$residuals)){ 
      w.resid <- residuals } else { warning("residuals argument to plot.gam is wrong length: ignored") }
    partial.resids <- TRUE
  } else { partial.resids <- residuals } # use working residuals or none
  
  m <- length(x$smooth) # number of smooth effects
  
  if (length(scheme)==1) scheme <- rep(scheme, m)
  if (length(scheme)!=m) { 
    warning( paste("scheme should be a single number, or a vector with", m, "elements") )
    scheme <- rep(scheme[1], m)
  }
  
  ## Array giving the order of each parametric term
  order <- if (is.list(x$pterms)){ unlist(lapply(x$pterms,attr,"order")) } else { attr(x$pterms,"order") }
  
  # Plot parametric terms as well?
  if (all.terms){ n.para <- sum(order==1) } else { n.para <- 0 } 
  
  if (se) { # Sort out CI widths for 1D and 2D smooths
    if (is.numeric(se)) { se2.mult <- se1.mult <- se } else { se1.mult <- 2; se2.mult <- 1} 
    if (se1.mult<0) { se1.mult<-0 }
    if (se2.mult < 0) { se2.mult <- 0 }
  } else { se1.mult <- se2.mult <-1 }
  
  if (se && x$Vp[1,1] < 0){  # Check that variances are actually available
    se <- FALSE
    warning("No variance estimates available")
  }
  
  if (partial.resids) { # Getting information needed for partial residuals
    if (is.null(w.resid)) { # produce working residuals if info available
      if (is.null(x$residuals)||is.null(x$weights)){ partial.resids <- FALSE } else {
        wr <- sqrt(x$weights)
        w.resid <- x$residuals*wr/mean(wr) # weighted working residuals
      }
    }
    if (partial.resids){ fv.terms <- predict(x,type="terms") } # get individual smooth effects
  }
  
  #######
  ## START: loop to get the data for the plots
  #######
  pd <- list(); # List of data to be plotted
  i <- 1 # needs a value if no smooths, but parametric terms ...
  if (m>0) for (i in 1:m) { ## work through smooth terms
    first <- x$smooth[[i]]$first.para
    last <- x$smooth[[i]]$last.para
    edf <- sum(x$edf[first:last]) ## Effective DoF for this term
    term.lab <- sub.edf(x$smooth[[i]]$label, edf)
    #P <- plot(x$smooth[[i]],P=NULL,data=x$model,n=n,n2=n2,xlab=xlab,ylab=ylab,too.far=too.far,label=term.lab,
    #          se1.mult=se1.mult,se2.mult=se2.mult,xlim=xlim,ylim=ylim,main=main,scheme=scheme[i],...)
    attr(x$smooth[[i]], "coefficients") <- x$coefficients[first:last] # Relevant coeffs for i-th smooth
    P <- .prepare.mgcv.smooth(x$smooth[[i]], data=x$model, partial.resids=partial.resids,
                              rug=rug, se=se, scale=scale, n=n, n2=n2,
                              pers=pers,theta=theta,phi=phi,jit=jit,xlab=xlab,ylab=ylab,main=main,label=term.lab,
                              ylim=ylim,xlim=xlim,too.far=too.far,shade=shade,shade.col=shade.col,
                              se1.mult=se1.mult,se2.mult=se2.mult,shift=shift,trans=trans,
                              by.resids=by.resids,scheme=scheme[i],inter=inter,...)
    
    if (is.null(P)) pd[[i]] <- list(plot.me=FALSE) else if (is.null(P$fit)) {
      p <- x$coefficients[first:last]   ## relevent coefficients 
      offset <- attr(P$X,"offset")      ## any term specific offset
      ## get fitted values ....
      if (is.null(offset)) P$fit <- P$X%*%p else P$fit <- P$X%*%p + offset 
      if (!is.null(P$exclude)) P$fit[P$exclude] <- NA
      if (se && P$se) { ## get standard errors for fit
        ## test whether mean variability to be added to variability (only for centred terms)
        if (seWithMean && attr(x$smooth[[i]],"nCons")>0) {
          if (length(x$cmX) < ncol(x$Vp)) x$cmX <- c(x$cmX,rep(0,ncol(x$Vp)-length(x$cmX)))
          X1 <- matrix(x$cmX,nrow(P$X),ncol(x$Vp),byrow=TRUE)
          meanL1 <- x$smooth[[i]]$meanL1
          if (!is.null(meanL1)) X1 <- X1 / meanL1
          X1[,first:last] <- P$X
          se.fit <- sqrt(pmax(0,rowSums((X1%*%x$Vp)*X1)))
        } else se.fit <- ## se in centred (or anyway unconstained) space only
            sqrt(pmax(0,rowSums((P$X%*%x$Vp[first:last,first:last,drop=FALSE])*P$X)))
        if (!is.null(P$exclude)) P$se.fit[P$exclude] <- NA
      } ## standard errors for fit completed
      if (partial.resids) { P$p.resid <- fv.terms[,length(order)+i] + w.resid }
      if (se && P$se) P$se <- se.fit*P$se.mult  # Note multiplier
      P$X <- NULL
      P$plot.me <- TRUE
      pd[[i]] <- P; rm(P) 
    } else { ## P$fit created directly
      if (partial.resids) { P$p.resid <- fv.terms[,length(order)+i] + w.resid }
      P$plot.me <- TRUE
      pd[[i]] <- P; rm(P)
    }
  } ## end of data setup loop through smooths
  #######
  ## STOP: loop to get the data for the plots
  #######
  
  
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
      .ggobj[[i]] <- .plot.mgcv.smooth(x$smooth[[i]],P=pd[[i]],partial.resids=partial.resids,rug=rug,se=se,
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
    
    
  } ## end of smooth plotting loop
  
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
