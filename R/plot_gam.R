#' #' Basic GAM plotting
#' #' 
#' #' 
#' #' @description Takes a fitted gam object produced by [mgcv::gam()] and plots 
#' #'   the component smooth functions that make it up, on the scale of the linear 
#' #'   predictor. Optionally produces term plots for parametric model components
#' #'   as well.
#' #' @param x A fitted gam object as produced by [mgcv::gam()].
#' #' @param residuals If TRUE then partial residuals are added to plots of 1-D 
#' #'   smooths. If FALSE then no residuals are added. If this is an array of the 
#' #'   correct length then it is used as the array of residuals to be used for 
#' #'   producing partial residuals. If TRUE then the residuals are the working 
#' #'   residuals from the IRLS iteration weighted by the IRLS weights. Partial
#' #'   residuals for a smooth term are the residuals that would be obtained by
#' #'   dropping the term concerned from the model, while leaving all other 
#' #'   estimates fixed (i.e. the estimates for the term plus the residuals).
#' #' @param rug When TRUE (default) then the covariate to which the plot applies 
#' #'   is displayed as a rug plot at the foot of each plot of a 1-d smooth, and
#' #'   the locations of the covariates are plotted as points on the contour plot 
#' #'   representing a 2-d smooth. Setting to FALSE will speed up plotting for
#' #'   large datasets.
#' #' @param se When TRUE (default) upper and lower lines are added to the 1-d 
#' #'   plots at 2 standard errors above and below the estimate of the smooth being
#' #'   plotted while for 2-d plots, surfaces at +1 and -1 standard errors are 
#' #'   contoured and overlayed on the contour plot for the estimate. If a positive
#' #'   number is supplied then this number is multiplied by the standard errors
#' #'   when calculating standard error curves or surfaces. See also shade, below.
#' #' @param pages (Default 0) the number of pages over which to spread the output.
#' #'   For example, if `pages = 1` then all terms will be plotted on one page with
#' #'   the layout performed automatically. Set to 0 to have the routine leave all
#' #'   graphics settings as they are.
#' #' @param select Allows the plot for a single model term to be selected for
#' #'   printing. e.g. if you just want the plot for the second smooth term set
#' #'   `select = 2`.
#' #' @param scale Set to -1 (default) to have the same y-axis scale for each plot,
#' #'   and to 0 for a different y axis for each plot. Ignored if ylim supplied.
#' #' @param n Number of points used for each 1-d plot - for a nice smooth plot
#' #'   this needs to be several times the estimated degrees of freedom for the
#' #'   smooth. Default value 100.
#' #' @param n2 Square root of number of points used to grid estimates of 2-d
#' #'   functions for contouring.
#' #' @param pers Set to TRUE if you want perspective plots for 2-d terms.
#' #' @param theta One of the perspective plot angles.
#' #' @param phi The other perspective plot angle.
#' #' @param jit Set to TRUE if you want rug plots for 1-d terms to be jittered.
#' #' @param xlab If supplied then this will be used as the x label for all plots.
#' #' @param ylab If supplied then this will be used as the y label for all plots.
#' #' @param main Used as title (or z axis label) for plots if supplied.
#' #' @param ylim If supplied then this pair of numbers are used as the y limits
#' #'   for each plot.
#' #' @param xlim If supplied then this pair of numbers are used as the x limits
#' #'   for each plot.
#' #' @param too.far If greater than 0 then this is used to determine when a
#' #'   location is too far from data to be plotted when plotting 2-D smooths. This
#' #'   is useful since smooths tend to go wild away from data. The data are scaled
#' #'   into the unit square before deciding what to exclude, and too.far is a
#' #'   distance within the unit square. Setting to zero can make plotting faster
#' #'   for large datasets, but care then needed with interpretation of plots.
#' #' @param all.terms If set to TRUE then the partial effects of parametric model 
#' #'   components are also plotted, via a call to [termplot]. Only terms of order 1
#' #'   can be plotted in this way.
#' #' @param shade Set to TRUE to produce shaded regions as confidence bands for 
#' #' smooths (not avaliable for parametric terms, which are plotted using [termplot]).
#' #' @param shade.col Define the color used for shading confidence bands.
#' #' @param shift Constant to add to each smooth (on the scale of the linear 
#' #' predictor) before plotting. Can be useful for some diagnostics, or with `trans`.
#' #' @param trans Function to apply to each smooth (after any shift), before plotting. 
#' #' `shift` and `trans` are occasionally useful as a means for getting plots on the 
#' #' response scale, when the model consists only of a single smooth.
#' #' @param seWithMean If TRUE the component smooths are shown with confidence 
#' #' intervals that include the uncertainty about the overall mean. If FALSE then 
#' #' the uncertainty relates purely to the centred smooth itself. Marra and Wood (2012) 
#' #' suggests that TRUE results in better coverage performance, and this is also 
#' #' suggested by simulation.
#' #' @param unconditional If TRUE then the smoothing parameter uncertainty corrected 
#' #' covariance matrix is used to compute uncertainty bands, if available. Otherwise 
#' #' the bands treat the smoothing parameters as fixed.
#' #' @param by.resids Should partial residuals be plotted for terms with by variables? 
#' #' Usually the answer is no, they would be meaningless.
#' #' @param scheme Integer or integer vector selecting a plotting scheme for each plot. 
#' #' See details. 
#' #' @param draw Should plots be drawn ? Default is TRUE.
#' #' @param inter Should plots be interactive when available ? Default is FALSE.
#' #' @param ... Other graphics parameters to pass on to plotting commands. 
#' #' See details for smooth plot specific options.
#' #' @importFrom plotly ggplotly
#' #' @import viridis
#' #' @name plot.gam
#' #' @details ...
#' #' @examples 
#' #' library(mgcViz)
#' #' 
#' #' @rdname plot.gam
#' #' @export plot.gam
#' plot.gam <- function(x, residuals = FALSE, rug = TRUE, se = TRUE, pages = 0, select = NULL,
#'                      scale = TRUE, n = 100, n2 = 40, pers = FALSE, theta = 30, phi = 30,
#'                      jit = FALSE, xlab = NULL, ylab = NULL, main = NULL,
#'                      ylim = NULL, xlim = NULL, too.far = 0.1, all.terms = FALSE,
#'                      shade = FALSE, shade.col = "gray80", shift = 0, trans = I,
#'                      seWithMean = FALSE, unconditional = FALSE, by.resids = FALSE,
#'                      scheme = 0, draw = TRUE, inter = FALSE, ...) { 
#'   m <- length(x$smooth) # number of smooth effects
#'   if (length(scheme) == 1) {
#'     scheme <- rep(scheme, m)
#'   }
#'   if (length(scheme) != m) {
#'     # change warning to error ?
#'     warning(paste("scheme should be a single number, or a vector with", m, "elements"))
#'     scheme <- rep(scheme[1], m)
#'   }
#'   fv.terms <- NULL
#'   # TODO: fix the following
#'   # .initializeXXX with x ??
#'   # also resDen isn't defined yet
#'   init <- .initializeXXX(x, unconditional, residuals, resDen, se, fv.terms)
#'   # affect initialize output
#'   o <- init$o
#'   w.resid <- init$w.resid
#'   partial.resids <- init$partial.resids
#'   se2.mult <- init$se2.mult
#'   se1.mult <- init$se1.mult
#'   se <- init$se
#'   fv.terms <- init$fv.terms
#'   order <- init$order
#'   # mapply instead of loop
#'   # TODO: fix fitSmooth = fv.terms in mapply call
#'   tmp <- mapply(FUN = .createP,
#'                 sm = x$smooth, scheme = scheme,
#'                 fitSmooth = split(fv.terms, rep(1:ncol(fv.terms), each = nrow(fv.terms))),
#'                 MoreArgs  = list(
#'                   x = x, partial.resids = partial.resids,
#'                   rug = rug, se = se, scale = scale, n = n, n2 = n2,
#'                   pers = pers, theta = theta, phi = phi, jit = jit, xlab = xlab, ylab = ylab,
#'                   main = main, label = term.lab, ylim = ylim, xlim = xlim, too.far = too.far, 
#'                   shade = shade, shade.col = shade.col, se1.mult = se1.mult, se2.mult = se2.mult,
#'                   shift = shift, trans = trans, by.resids = by.resids, 
#'                   seWithMean = seWithMean,  w.resid = w.resid, inter = inter, ...
#'                 ))
#'   pd <- lapply(tmp, `[[`, "P")
#'   lapply(seq_along(tmp), function(ii) {
#'     attr(x$smooth[[ii]], "coefficients") <<- tmp[[ii]][["coef"]]
#'   })
#'   # Plot parametric terms as well?
#'   n.para <- ifelse(all.terms, sum(order == 1), 0)
#'   ##############################################
#'   ## sort out number of pages and plots per page 
#'   ##############################################
#'   n.plots <- n.para
#'   if (m > 0) {
#'     for (i in 1:m) {
#'       n.plots <- n.plots + as.numeric(pd[[i]]$plot.me)
#'     }
#'   } 
#'   if (n.plots == 0) {
#'     stop("No terms to plot - nothing for plot.gam() to do.")
#'   }
#'   # TODO: give warning if pb with pages args ?
#'   if (pages > n.plots) {
#'     pages <- n.plots
#'   }
#'   if (pages < 0) {
#'     pages <- 0
#'   }
#'   if (pages != 0)    # figure out how to display things
#'   { ppp<-n.plots%/%pages
#'   if (n.plots%%pages!=0)
#'   { ppp<-ppp+1
#'   while (ppp*(pages-1)>=n.plots) pages<-pages-1
#'   }
#'   
#'   # now figure out number of rows and columns
#'   c <- r <- trunc(sqrt(ppp))
#'   if (c<1) r <- c <- 1
#'   if (c*r < ppp) c <- c + 1
#'   if (c*r < ppp) r <- r + 1
#'   oldpar<-par(mfrow=c(r,c))
#'   
#'   } else
#'   { ppp<-1;oldpar<-par()}
#'   
#'   if ((pages==0&&prod(par("mfcol"))<n.plots&&dev.interactive())||
#'       pages>1&&dev.interactive()) ask <- TRUE else ask <- FALSE
#'   
#'   if (!is.null(select)) {
#'     ask <- FALSE
#'   }
#'   
#'   if (ask) {
#'     oask <- devAskNewPage(TRUE)
#'     on.exit(devAskNewPage(oask))
#'   }
#'   
#'   #####################################
#'   ## get a common scale, if required...
#'   #####################################
#'   if (scale==TRUE && is.null(ylim)) {
#'     k <- 0
#'     if (m>0) for (i in 1:m) if (pd[[i]]$plot.me&&pd[[i]]$scale) { ## loop through plot data 
#'       if (se&&length(pd[[i]]$se)>1) { ## require CIs on plots
#'         ul<-pd[[i]]$fit+pd[[i]]$se
#'         ll<-pd[[i]]$fit-pd[[i]]$se
#'         if (k==0) { 
#'           ylim <- c(min(ll,na.rm=TRUE),max(ul,na.rm=TRUE));k <- 1
#'         } else {
#'           if (min(ll,na.rm=TRUE)<ylim[1]) ylim[1] <- min(ll,na.rm=TRUE)
#'           if (max(ul,na.rm=TRUE)>ylim[2]) ylim[2] <- max(ul,na.rm=TRUE)
#'         }
#'       } else { ## no standard errors
#'         if (k==0) {
#'           ylim <- range(pd[[i]]$fit,na.rm=TRUE);k <- 1
#'         } else {
#'           if (min(pd[[i]]$fit,na.rm=TRUE)<ylim[1]) ylim[1] <- min(pd[[i]]$fit,na.rm=TRUE)
#'           if (max(pd[[i]]$fit,na.rm=TRUE)>ylim[2]) ylim[2] <- max(pd[[i]]$fit,na.rm=TRUE)
#'         }
#'       }
#'       if (partial.resids) { 
#'         ul <- max(pd[[i]]$p.resid,na.rm=TRUE)
#'         if (ul > ylim[2]) ylim[2] <- ul
#'         ll <-  min(pd[[i]]$p.resid,na.rm=TRUE)
#'         if (ll < ylim[1]) ylim[1] <- ll
#'       } ## partial resids done
#'     } ## loop end 
#'     ylim <- trans(ylim+shift)
#'   } ## end of common scale computation
#'   
#'   ##############################################################
#'   ## now plot smooths, by calling plot methods with plot data...
#'   ##############################################################
#'   
#'   .ggobj <- list()
#'   if (m>0) for (i in 1:m) if (pd[[i]]$plot.me&&(is.null(select)||i==select)) {
#'     withCallingHandlers({
#'       .ggobj[[i]] <- .plot(x$smooth[[i]],P=pd[[i]],partial.resids=partial.resids,rug=rug,se=se,
#'                            scale=scale,n=n,n2=n2,pers=pers,theta=theta,phi=phi,jit=jit,xlab=xlab,
#'                            ylab=ylab,main=main,ylim=ylim,xlim=xlim,too.far=too.far,shade=shade,
#'                            shade.col=shade.col,shift=shift,trans=trans,by.resids=by.resids,scheme=scheme[i], 
#'                            inter=inter, ...)
#'       if(draw){ if(inter){print(ggplotly(.ggobj[[i]]+theme_bw()))}else{print(.ggobj[[i]]+theme_bw())} }
#'     }, warning = function(w) {
#'       if (length(grep("Ignoring unknown parameters: ", conditionMessage(w))))
#'       {
#'         invokeRestart("muffleWarning")
#'       }
#'     })
#'   }
#'   
#'   ####################################################
#'   ## Finally deal with any parametric term plotting...
#'   ####################################################
#'   
#'   if (n.para>0) # plot parameteric terms
#'   { class(x) <- c("gam","glm","lm") # needed to get termplot to call model.frame.glm 
#'   if (is.null(select)) {
#'     attr(x,"para.only") <- TRUE
#'     termplot(x,se=se,rug=rug,col.se=1,col.term=1,main=attr(x$pterms,"term.labels"),...)
#'   } else { # figure out which plot is required
#'     if (select > m) { 
#'       ## can't figure out how to get this to work with more than first linear predictor
#'       ## as termplots relies on matching terms to names in original data... 
#'       select <- select - m # i.e. which parametric term
#'       term.labels <- attr(x$pterms,"term.labels")
#'       term.labels <- term.labels[order==1]
#'       if (select <= length(term.labels)) {
#'         # if (interactive() && m &&i%%ppp==0) 
#'         termplot(x,terms=term.labels[select],se=se,rug=rug,col.se=1,col.term=1,...)
#'       }  
#'     }
#'   }
#'   }
#'   if (pages>0) par(oldpar)
#'   attr(.ggobj, "rawData") <- pd
#'   invisible(.ggobj)
#' } ## end plot.gam
