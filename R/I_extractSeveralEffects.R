######
# Internal function that extracts several parametric or smooth effects
.extractSeveralEffects <- function(.x, .sel, .allT){
  
  if( inherits(.x, "mgamViz") ){
    nsm <- length(.x[[1]]$smooth)
    npr <- .x[[1]]$store$np # number of parametric terms
  } else {
    nsm <- length(.x$smooth)
    npr <- .x$store$np      # number of parametric terms
  }
  
  if( is.null(.sel) ) { 
    .sel <- if( .allT ) { 
      1:(nsm+npr) 
    } 
    else { 
      if(nsm > 0){ 1:nsm } else { return(NULL) } 
    } 
  }
  
  selS <- .sel[ .sel <= nsm ]
  selP <- .sel[ .sel > nsm & .sel <= (nsm+npr) ]
  
  smo <- list()
  # Extract smooths
  if( length(selS) ) { smo <- lapply(selS, sm, o = .x) }
  
  # Add also parametric terms
  return( c(smo, lapply(selP-nsm, pterm, o = .x)) )
}