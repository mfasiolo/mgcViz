#############
#' Printing the output of plot.gamViz
#' 
#' @description This method prints the output of [plot.gamViz].
#' @param x an object of class \code{plotGam}.
#' @param ask should we ask before moving from one page to the next?
#' @param pages the number of pages over which to spread the output.
#' @param addLay if TRUE, and if the \code{$empty} slot of the \code{plotGam} object is TRUE,  
#'               we add some default layers to the plots, before printing. Does not have
#'               any affect if the \code{plotGam} object already contains some layers.
#' @param ... further arguments to be passed to \code{grid.arrange}.
#' @return Returns the output of [gridExtra::grid.arrange], invisibly.
#' @name print.plotGam
#' @rdname print.plotGam
#' @importFrom gridExtra grid.arrange
#' @export print.plotGam
#' @export
#' 
print.plotGam <- function(x, ask = TRUE, pages = NULL, addLay = TRUE, ...){
  
  .addDefaultLayers <- function( .l ){
    .cl <- paste(.l$type, collapse = '')
    .l <- switch(.cl, 
                 "fs1D" = .l + l_fitLine() + theme(legend.position="none"),
                 "1D" = .l + l_fitLine() + l_ciLine(),
                 "2D" = .l + l_fitRaster() + l_fitContour(), 
                 "MD" = .l + l_fitRaster() + l_fitContour(), 
                 "MDslice" = .l + l_fitRaster() + l_fitContour(), 
                 "sos0" = .l + l_fitRaster() + l_fitContour(), 
                 "sos1" = .l + l_fitRaster() + l_fitContour(),
                 "randomEffect" = .l + l_fitLine() + l_ciLine() + l_points(), 
                 "MultiRandomEffect" = .l + l_fitLine() + l_ciLine() + l_points(), 
                 "mrf" = .l + l_poly(), 
                 "PtermNumeric" = .l + l_fitLine() + l_ciLine(),
                 "PtermFactor" = .l + l_ciBar() + l_fitPoints(),
                 "PtermLogical" = .l + l_ciBar() + l_fitPoints(), 
                 "Multi1D" = .l + l_fitLine(),
                 "Multi2D" = .l + l_fitRaster() + l_fitContour(),
                 "MultiPtermNumeric" = .l + l_ciBar() + l_fitPoints(),
                 "MultiPtermFactor" = .l + l_ciBar() + l_fitPoints()
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

