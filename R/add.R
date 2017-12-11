#'
#' Adding layers to smooth effect plots
#'
#' @param e1 an object of class \code{plotSmooth}.
#' @param e2 a plot component, as described below.
# #' @export
#' @method + plotSmooth
# #' @rdname plotSmooth-add
#' @noRd
#' @examples
#'
addPlotSmooth <- function(e1, e2) {
  
  # If e2 is a gamLayer, we need to call the corresponding layer internal.
  # This returns either a ggplot or a list of ggplots (with class listOfLayers)
  if( "gamLayer" %in% class(e2) ){
    e2$arg$data <- e1$data 
    fun <- tryCatch(get( paste(e2$fun, ".", paste(e1$type, collapse = ''), sep = '') ), 
                    error = function(e){
                      e <- conditionMessage(e)
                      if( grepl("not found", e) ){
                        warning(paste("No ", e2$fun, "() layer available for type \"", 
                                      paste(e1$type, collapse = ' '), "\"", sep = ''))
                        return( function(...) NULL )
                      } else {
                        stop(e)
                      }
                    })
    e2 <- fun(a = e2$arg)
  }
  
  # If e2 is a "listOfLayers" (list of ggplots) add them one by one.
  # Calling directly ggplot %+% here.
  if( "listOfLayers" %in% class(e2) ){
    for(ii in 1:length(e2)){
      e1$ggObj <- e1$ggObj %+% e2[[ii]]
    } 
  } else {
    e1$ggObj <- e1$ggObj %+% e2
  }
  
  return( e1 )
}


#'
#' Add layers to output of plot.gamViz
#'
#' @param e1 an object of class \code{plotGam}.
#' @param e2 a plot component, as described below.
# #' @export
#' @method + plotGam
# #' @rdname plotGam-add
#' @noRd
#' @examples
#'
addPlotGam <- function(e1, e2) {
  
  # Add layer `e2` to each plot is `e1`. If `+` given an error, don't add `e2` to that plot.
  e1$plots <- lapply(e1$plots, 
                     function(.l){
                       return( withCallingHandlers(.l + e2,
                                                   warning = function(w){ 
                                                     if(any(grepl("layer available for type", w))){ 
                                                       invokeRestart( "muffleWarning" )
                                                     }
                                                   }) )
                     })
  
  # If we added a "gamLayer" or a "listOfLayers" we consider the object to be non-empty
  # so that print.plotGam() will not add any layer.
  if( inherits(e2, "gamLayer") || inherits(e2, "listOfLayers") ){
    
    e1$empty <- FALSE
    
  }
  
  return( e1 )
}


#' @export
"+.gg" <- function(e1, e2) {
  
  if( inherits(e1, "plotSmooth") ){
    
    return( addPlotSmooth(e1, e2) )
    
  } else { 
    
    if( inherits(e1, "plotGam") ){
      
      return( addPlotGam(e1, e2) )
      
    } else { # ggplot2 addition %+%
      
      return( e1 %+% e2 )
      
    }
    
  } 
  
}

#' #' @rdname plotSmooth-add
#' #' @export
#' "%+%" <- `+.plotSmooth`
