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
  
  if( inherits(o, "mgamViz") ){
    out <- lapply(o, sm, select = select)
    class(out) <- paste0("multi.", class(out[[1]]))
    return( out )
  }
  
  if( !inherits(o, "gamViz") ){ stop("Argument 'o' should be of class 'gamViz'. See ?getViz") }
  
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
  
  class(out) <- cl
  
  return( out )
  
}
