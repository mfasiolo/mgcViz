#############
#' Printing output of mgcViz::plot.gam
#' 
#' @description XXX
#' @param x an object of class \code{plotGam}.
#' @param ask should we ask before moving from one page to the next?
#' @param pages the number of pages over which to spread the output.
#' @param addLay if TRUE, and if the \code{$empty} slot of the \code{plotGam} object is TRUE,  
#'               we add some default layers to the plots, before printing. Does not have
#'               any affect if the \code{plotGam} object already contains some layers.
#' @param ... currently unused.
#' @name print.check.gam
#' @rdname print.check.gam
#' @importFrom gridExtra grid.arrange
#' @export 
#' 
print.plotGam <- function(x, ask = TRUE, pages = NULL, addLay = TRUE, ...){
  
  .addDefaultLayers <- function( .l ){
    .cl <- paste(class(.l), collapse = '')
    .l <- switch(.cl, 
                 "plotSmoothfs1Dgg" = .l + l_fitLine(),
                 "plotSmooth1Dgg" = .l + l_fitLine() + l_ciLine(),
                 "plotSmooth2Dgg" = .l + l_fitRaster() + l_fitContour(), 
                 "plotSmoothMDgg" = .l + l_fitRaster() + l_fitContour(), 
                 "plotSmoothsos0gg" = .l + l_fitRaster() + l_fitContour(), 
                 "plotSmoothsos1gg" = .l + l_fitRaster() + l_fitContour(),
                 "plotSmoothrandomEffectgg" = .l + l_fitLine() + l_ciLine() + l_points() 
                 )
    
    return( .l )
  }
  
  # If plots have no layers, we add some default ones
  if( addLay && x$empty ){
    
    x$plots <- lapply(x$plots, 
                       function(.l){
                         .addDefaultLayers(.l)
                       })
    
  }
  
  n <- length(x$plots)
  if( is.null(pages) ){ pages <- n }
  
  # Determined layout of grid of plots
  la <- .detNumPages(n, pages)
  
  ask <- ask && la$npag>1
  
  # Print each page
  # indx: the indexes of the plots to rendered in each array
  # tmp: we need to extract the `ggplot` objects to be fed to grid.arrange()
  for(ii in 1:la$npag){
    
    if(ask) { readline(prompt = "Hit <Return> to see next plot:") }
    indx <- ((ii-1)*la$ppp+1) : min(ii*la$ppp, n)
    tmp <- lapply(indx, function(.kk) x$plots[[.kk]]$ggObj)
    grid.arrange(grobs = tmp, ncol = la$c, nrow = la$r, ...)
    
  }
  
  return( invisible(NULL) )
  
}

