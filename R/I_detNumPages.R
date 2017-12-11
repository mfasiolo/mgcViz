###############
# Given the number of plots and the number of pages, determines the
# - number of pages, which might have been wrong (eg > than number of plots)
# - number of plots per page
# - number of columns
# - number of rows
#
.detNumPages <- function(.npl, .npag){
  
  if (.npl==0) stop("No terms to plot - nothing for plot.gamViz() to do.")
  if ( .npag < 0 ) stop("Number of pages < 0")
  
  if ( .npag > .npl ) { .npag <- .npl }
  
  
  # START: Figure out how to display things
  if (.npag != 0) { 
    
    # Fin number of plots per page
    ppp <- .npl %/% .npag
    if (.npl %% .npag != 0) { 
      ppp <- ppp + 1
      while ( ppp * (.npag - 1) >= .npl ) { .npag <- .npag-1 }
    } 
    
    # Now figure out number of rows and columns
    c <- r <- trunc( sqrt( ppp ) )
    if (c<1) { r <- c <- 1 }
    if (c*r < ppp) { c <- c + 1 }
    if (c*r < ppp) { r <- r + 1 }
    
  } else { 
    ppp <- r <- c <- .npag <- 1; # One plot per page
  } # END
  
  return( list("npag" = .npag, "ppp" = ppp, "c" = c, "r" = r) )
  
}
