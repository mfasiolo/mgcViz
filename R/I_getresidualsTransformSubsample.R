
## Internal function used by check0D, check1D and check2D
#
.getresidualsTransformSubsample <- function(o, y, maxpo, trans, type){
  
  # a) Transform simulated responses to residuals (unless type == "y")
  sim <- o$store$sim
  if( !is.null(sim) ){
    if( type != "y" ){
      sim <- aaply(sim, 1, 
                   function(.yy){  
                     o$y <- .yy
                     return( residuals(o, type = type) )
                   }) 
    }
  }
  
  # b) Apply optional transformation to observed and simulated y's
  if( !is.null(trans) ){
    y <- as.vector(trans( y ))
    if( !is.null(sim) ) { sim <- t(apply(sim, 1, trans)) }
  }
  
  # c) Sample if too many points (> maxpo) 
  m <- length( as.vector(y) )
  sub <- if(m > maxpo) { 
    sample( c(rep(TRUE, maxpo), rep(FALSE, m-maxpo)) )
  } else { 
    rep(TRUE, m) 
  }
  
  return( list("sim" = sim, "y" = y, "sub" = sub) )
  
}
