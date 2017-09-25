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


#' #' @rdname plotSmooth-add
#' #' @export
#' "%+%" <- `+.plotSmooth`
