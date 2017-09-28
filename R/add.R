#'
#' Add components smooth effect plot
#'
#' @param e1 an object of class \code{plotSmooth}.
#' @param e2 a plot component, as described below.
#' @export
#' @method + plotSmooth
#' @rdname plotSmooth-add
#' @examples
#'
"+.plotSmooth" <- function(e1, e2) {
  
  if( "gamTheme" %in% class(e2) ){
    class(e2) <- c("theme", "gg")
  }
  
  if( "gamLayer" %in% class(e2) ){
    e2$arg$data <- e1$data 
    fun <- get( paste(e2$fun, ".", paste(class(e1), collapse = ''), sep = '') )
    e2 <- fun(a = e2$arg)
  }
  
  if( "listOfLayers" %in% class(e2) ){
    for(ii in 1:length(e2)){
      e1$ggObj <- e1$ggObj + e2[[ii]]
    } 
  } else {
    e1$ggObj <- e1$ggObj + e2
  }
  
  return( e1 )
}


#'
#' Add layers to output of plot.gam
#'
#' @param e1 an object of class \code{plotGam}.
#' @param e2 a plot component, as described below.
#' @export
#' @method + plotGam
#' @rdname plotGam-add
#' @examples
#'
"+.plotGam" <- function(e1, e2) {
  
  # Add layer `e2` to each plot is `e1`. If `+` given an error, don't add `e2` to that plot.
  e1$plots <- lapply(e1$plots, 
                     function(.l){
                       return( tryCatch(.l + e2, error = function(.e) .l) )
                     })
  
  # If we added a "gamLayer" or a "listOfLayers" we consider the object to be non-empty
  # so that print.plotGam() will not add any layer.
  if( "gamLayer" %in% class(e2) || "listOfLayers" %in% class(e2) ){
    
    e1$empty <- FALSE
    
  }
  
  return( e1 )
}


#' #' @rdname plotSmooth-add
#' #' @export
#' "%+%" <- `+.plotSmooth`
