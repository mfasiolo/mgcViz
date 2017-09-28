#############################################
#' Basic GAM plotting
#'
#' @description This function overwrites \code{mgcv::plot.gam()}.
#' 
#' @param o A fitted gam object as produced by \code{mgcv::gam()} or a \code{gamViz} object, 
#'          produced by \code{mgcViz::getViz()}.
#' @param n number of points used for each 1-d plot. For a nice smooth plot 
#'          this needs to be several times the estimated degrees of freedom for the smooth.
#' @param n2 square root of number of points used to grid estimates of 2D functions 
#'           for plotting contours or heatmaps.
#' @param select allows plotting a subset of model term. For instance, if you just want the plot
#'             for the second smooth term set \code{select = 2}.
#' @param ... other parameters, such as \code{maxpo} or \code{trans} to be passed to the specific
#'            plotting methods for each effect (e.g. to \code{plot.mgcv.smooth.1D}).
#' @name plot.gam
#' @examples
#' 
#' library(mgcViz)
#'
#' ######## Basic example
#' # Simulate some data and fit model
#' set.seed(2)  
#' dat <- gamSim(1,n=1e3,dist="normal",scale=2)
#' b <- bam(y~s(x0)+s(x1, x2)+s(x3), data=dat)
#' 
#' # Plotting as mgcv::plot.gam() would
#' plot(b)
#' 
#' # Now on one page and with out title on the second plot
#' print(plot(b) + labs(title = NULL), pages = 1) 
#' 
#' # So far we used default layers, added in the printing phase, but
#' # we might want to specify our own layers. Here we is how to do it
#' pl <- plot(b) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
#'   l_ciLine(colour = 2) + wrapTheme(theme_get()) + labs(title = NULL)
#' print(pl, pages = 1)
#' 
#' # We might want to plot only the first smooth
#' plot(b, select = 1) + l_dens(type = "cond") + l_fitLine() + l_ciLine()
#' 
#' ######## Example with "by variable" smooth effect
#' # Simulate data and fit model
#' dat <- gamSim(4)
#' b <- gam(y ~ fac+s(x2,by=fac)+s(x0),data=dat)
#' # print() only needed because we want to plot on a single page
#' print(plot(b), pages = 1) 
#' 
#' ######## Example with 3D smooth effect which cannot be plotted
#' # Simulate data and fit model
#' n <- 5e3
#' x <- rnorm(n); y <- rnorm(n); z <- rnorm(n); z2 <- rnorm(n)
#' 
#' ob <- (x-z)^2 + (y-z)^2 + z2^3 + rnorm(n)
#' b1 <- bam(ob ~ s(x, y, z) + s(z2), discrete = T)
#' b1 <- getViz(b1)
#' 
#' # Only second effect get plotted
#' plot.gam(b1)
#' # In fact this does not plot anything
#' plot.gam(b1, select = 1)
#' # For plotting effects with more than 2D, one we need specific method. 
#' # See ?plot.mgcv.smooth.MD
#' 
#' @rdname plot.gam
#' @export plot.gam
#'
plot.gam <- function(o, n = 100, n2 = 40, select = NULL, ...) {
  
  if( !("gamViz" %in% class(o)) ) { o <- getViz(o)  }
  
  nsm <- length(o$smooth)
  if( is.null(select) ) { select <- 1:nsm }
  if( any(select > nsm) ) { 
    stop(paste("No element of `select` can be greater than ", nsm, ", the number of smooths.", sep=''))
  }
  
  # Extract smooths
  smo <- lapply(select, sm, o = o)
  
  # Wrapper function to plot each smooth
  wrap <- function(.smo, .n, .n2, ...){
    
    if( "mgcv.smooth.MD" %in% class(.smo) ) { return(NULL) }
    if( "mgcv.smooth.2D" %in% class(.smo) ) { .n <- .n2 }
    
    return( plot(o = .smo, n = .n, ...) )
  }
  
  # Plotting each smooth. If a plot is NULL we don't include it in the list `pls`.
  zz <- 1
  pls <- list()
  for(ii in 1:length(smo)){
    tmp <- wrap(smo[[ii]], .n = n, .n2 = n2, ...)
    if( !is.null(tmp) ){ 
      pls[[zz]] <- tmp
      zz <- zz + 1
    }
  }
  
  # Nothing to plot: probably because we have no plotting methods available.
  # This can happen when the model has only a smooth in 3 or more dimensions. 
  if( !length(pls) ){
    message("Nothing to plot!")
    return( invisible(NULL) )
  }
  
  out  <- structure(list("plots" = pls, "empty" = TRUE), 
                    "class" = "plotGam")
  
  return( out )
  
}



