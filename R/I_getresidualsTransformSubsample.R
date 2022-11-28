
## Internal function used by check0D, check1D and check2D
#
.getresidualsTransformSubsample <- function(o, y, maxpo, trans, type){
  
  # Dimensionality of response
  d <- dim(as.matrix(y))[2]
  
  # a) Transform simulated responses to residuals (unless type == "y")
  sim <- o$store$sim
  if( !is.null(sim) ){
    if(d == 1){ 
       sim <- llply(seq_len(ncol(sim)), function(ii) sim[ , ii])
    } 
    if( type != "y" ){
      sim <- llply(sim, 
                    function(.yy){  
                      o$y <- .yy
                      return( residuals(o, type = type) )
                    }) 
    }
    if( !is.null(trans) ){ sim <- llply(sim, trans) }
    if( !is.matrix(sim[[1]]) ){ sim <- llply(sim, as.matrix) }
  }
  
  # b) Apply optional transformation to observed and simulated y's
  if( !is.null(trans) ){ y <- trans( y ) }
  
  # c) Sample if too many points (> maxpo) 
  m <- nrow( as.matrix(y) )
  sub <- if(m > maxpo) { 
    sample( c(rep(TRUE, maxpo), rep(FALSE, m-maxpo)) )
  } else { 
    rep(TRUE, m) 
  }
  
  return( list("sim" = sim, "y" = y, "sub" = sub) )
}
