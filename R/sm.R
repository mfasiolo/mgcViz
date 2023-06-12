#'
#' Extracting a smooth effect from a GAM model
#' 
#' @description This function can be used to extract a smooth or random effect from an object of 
#'              class \code{gamViz}.
#' 
#' @param o an object of class \code{gamViz}, the output of a [getViz] call.
#' @param select index of the selected smooth or random effect.
#' @return An object representing a smooth effect.  
#' @seealso See [getViz] for examples.
#' @name sm
#' @rdname sm
#' @export sm
sm <- function(o, select){

  if( inherits(o, "list") ){
    if( all(sapply(o, function(.x){inherits(.x, "gamViz")})) == FALSE ){
      stop("Object \"o\" should be of class \"mgamViz\" or (a list of) \"gamViz\" objects")
    }
    if( is.null(names(o)) ){ names(o) <- 1:length(o) }
    class(o) <- "mgamViz"
  }
    
  if( inherits(o, "mgamViz") ){
    out <- lapply(o, sm, select = select)
    class(out) <- paste0("multi.", class(out[[1]]))
    attr(out, "isMQGAM") <- inherits(o, "mqgamViz") # Signal that 'o' is output of mqgamV 
    return( out )
  }
  
  if( !inherits(o, "gamViz") ){ stop("Argument 'o' should be of class 'gamViz'. See ?getViz") }
  
  # We should just use un-weighted raw residuals for plots
  if("qgam" %in% class(o)){
    o$residuals <- residuals(o, type = "response")
    o$weights <- o$weights * 0 + 1 
  }
  
  m <- length(o$smooth) # number of smooth effects
  
  if(length(select)>1){ stop("select should be a scalar") }
  if(select > m){ stop(paste("select should be smaller than", m, "the number of smooths in gamObject")) }
  
  out <- list("ism"=select, "gObj"=o)
  
  cl <- class( o$smooth[[select]] )
  
  if("mgcv.smooth" %in% cl){
    d <- o$smooth[[select]]$dim
    if(d > 2) { d <- "M" }
    cl[which(cl=="mgcv.smooth")] <- paste("mgcv.smooth.", d, "D", sep='')
  }
  
  if("fs.interaction" %in% cl){
    cl[which(cl=="fs.interaction")] <- paste("fs.interaction.", o$smooth[[select]]$dim, "D", sep='')
  }
  
  if("si.smooth" %in% cl){
    cl[which(cl=="si.smooth")] <- paste("si.smooth.", o$smooth[[select]]$dim, "D", sep='')
  }
  
  class(out) <- cl
  
  return( out )
  
}
