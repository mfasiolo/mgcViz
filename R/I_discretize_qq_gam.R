# Internal function that discretizes and prepares for plotting
# INPUT 
# - P a list, the output of .compute.qq.gam()
# - discrete if TRUE we are going to bin the residuals and interpolate their conf interv
# - ngr number of grid points
# - CI if TRUE we want to compute also conf interv
# - show.reps if TRUE we want to plot all simulated reps
# OUTPUT
# - P a list ready to be feed to .plot.qq.gam()
.discretize.qq.gam <- function(P, discrete, ngr, CI, show.reps){
  
  n <- length(P$Dq)
  rep <- ncol(P$dm)
  
  if(discrete && ngr >= n){ discrete <- FALSE }
  
  if( CI && is.null(P$conf) ){
    message("CI==TRUE but intervals have not been already computed by qq.gamViz. Setting CI to FALSE.")
    CI <- FALSE
  }
  
  if( discrete ){
    DqFull <- P$Dq
    tmp <- bin1D(x = P$Dq, y = P$D, ngr = ngr)
    P$D <- tmp$ym
    P$Dq <- tmp$xm
    
    if( show.reps && !is.null(P$dm) ){
      tmp <- lapply(1:ncol(P$dm), function(ii) bin1D(DqFull, P$dm[ , ii], ngr))
      dmx <- do.call("c", lapply(tmp, function(inp) inp$xm))
      dmy <- do.call("c", lapply(tmp, function(inp) inp$ym))
      id <- lapply(1:rep, function(ii) rep(ii, length(tmp[[ii]]$xm)))
      id <- do.call("c", id)
    }
    
    if( CI && !is.null(P$conf) ){
      P$conf <- rbind(approx(x=DqFull, y=P$conf[1, , drop=T], xout=P$Dq)$y, 
                      approx(x=DqFull, y=P$conf[2, , drop=T], xout=P$Dq)$y)
    }
  } else {
    if( show.reps && !is.null(P$dm) ){
      dmx <- rep(P$Dq, rep)
      dmy <- c( as.numeric(P$dm) )
      id <- rep(1:rep, each=n)
    }
  }
  
  if( show.reps && !is.null(P$dm) ){
    P$dm <- data.frame("x"=dmx, "y"=dmy, "id"=id)
  }
  
  if( CI && !is.null(P$conf) ){
    P$conf <- data.frame("x"= c(P$Dq, rev(P$Dq)), 
                         "y"= c(P$conf[1, , drop=T], rev(P$conf[2, , drop=T])))
  }
  
  return( P )
}
