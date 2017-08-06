#' Slicing an mgcv.smooth.MD object
#' 
#' @description XXX
#' @name slice.mgcv.smooth.MD
#' @rdname slice.mgcv.smooth.MD
#' @export 
slice.mgcv.smooth.MD <- function(o, fix, ...){
  
  nsl <- length(fix[[1]])
  
  if( any(sapply(fix, length) != nsl) ){ stop("All vectors in `fix` must have the same length") }
  
  # Produce slices
  plots <- lapply(1:nsl, 
                  function(.ii){
                    .vr <- sapply(fix, "[[", .ii)
                    plot(o, fix = .vr, ...)
                  })
  
  out <- structure(list("plots" = plots, "fix" = fix), class="smooth.slice.2D")
  
  return( out )
}