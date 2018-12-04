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
  
  mcls <- x$plots[[1]]$data$misc$modelClass
  
  .addDefaultLayers <- function( .l ){
    .cl <- paste(.l$type, collapse = '')
    
    if( grepl("Check", .cl) ){ # [A] Checking plots
      if("qgam" %in% mcls){ # a.1 Quantile GAMS
        .l <- switch(.cl, 
                     "Check0DScalarNumeric" = .l + l_hist() + l_vline(),
                     "Check0DScalarFactor" = .l + l_hist() + l_vline(),
                     "Check0DScalarLogical" = .l + l_hist() + l_vline(),
                     "Check0DVectorNumeric" = .l + l_hist(),
                     "Check0DVectorFactor" = .l + l_hist(),
                     "Check0DVectorLogical" = .l + l_hist(),
                     "Check1DNumeric" =  .l + l_gridQCheck1D() + l_rug(), 
                     "Check1DFactor" = .l + l_gridQCheck1D() + l_rug(),
                     "Check1DLogical" = .l + l_gridQCheck1D() + l_rug(),
                     "Check2DNumericNumeric" = .l + l_gridQCheck2D(),
                     "Check2DFactorFactor" = .l + l_gridQCheck2D() + l_rug(),
                     "Check2DFactorNumeric" = .l + l_gridQCheck2D() + l_rug(),
                     .l
        )
      } else { # a.2 Standard GAMS
        .l <- switch(.cl, 
                     "Check0DScalarNumeric" = .l + l_hist() + l_vline(),
                     "Check0DScalarFactor" = .l + l_hist() + l_vline(),
                     "Check0DScalarLogical" = .l + l_hist() + l_vline(),
                     "Check0DVectorNumeric" = .l + l_hist(),
                     "Check0DVectorFactor" = .l + l_hist(),
                     "Check0DVectorLogical" = .l + l_hist(),
                     "Check1DNumeric" =  .l + l_dens2D("cond") + l_gridCheck1D(mean, showReps = FALSE), 
                     "Check1DFactor" = .l + l_gridCheck1D(showReps = FALSE) + l_rug(),
                     "Check1DLogical" = .l + l_gridCheck1D(showReps = FALSE) + l_rug(),
                     "Check2DNumericNumeric" = .l + l_gridCheck2D(),
                     "Check2DFactorFactor" = .l + l_gridCheck2D() + l_rug(),
                     "Check2DFactorNumeric" = .l + l_gridCheck2D() + l_rug(),
                     .l
        )
      }
    } else { # [B] Smooth effect plots
    
    .l <- switch(.cl, 
                 "fs1D" = .l + l_fitLine() + theme(legend.position="none"),
                 "1D" = .l + l_fitLine() + l_ciLine(),
                 "2D" = .l + l_fitRaster() + l_fitContour(), 
                 "MD" = .l + l_fitRaster() + l_fitContour(), 
                 "MDslice" = .l + l_fitRaster() + l_fitContour(), 
                 "sos0" = .l + l_fitRaster() + l_fitContour(), 
                 "sos1" = .l + l_fitRaster() + l_fitContour(),
                 "randomEffect" = .l + l_fitLine() + l_ciLine() + l_points(), 
                 "MultiRandomEffect" = .l + l_points(), 
                 "mrf" = .l + l_poly(), 
                 "PtermNumeric" = .l + l_fitLine() + l_ciLine(),
                 "PtermFactor" = .l + l_ciBar() + l_fitPoints(),
                 "PtermLogical" = .l + l_ciBar() + l_fitPoints(), 
                 "Multi1D" = .l + l_fitLine(),
                 "Multi2D" = .l + l_fitRaster() + l_fitContour(),
                 "MultiPtermNumeric" = .l + l_ciBar() + l_fitPoints(),
                 "MultiPtermFactor" = .l + l_ciBar() + l_fitPoints(), 
                  .l
                 )
    
    }
    
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

