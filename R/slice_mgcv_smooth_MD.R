#' Slicing an mgcv.smooth.MD object
#' 
#' @description XXX
#' @name slice.mgcv.smooth.MD
#' @rdname slice.mgcv.smooth.MD
# #' @export 
slice.mgcv.smooth.MD <- function(o, fix, ...){
  
  l <- list(...)
  
  # 'noiseup' needed later by plot.smooth.slice.2D, hence we extract is here and store it in output object
  if( !is.null(l$noiseup) ){ noiseup <- l$noiseup } else { noiseup <- formals(plot.mgcv.smooth.MD)$noiseup }
  
  nsl <- length(fix[[1]])
  
  if( any(sapply(fix, length) != nsl) ){ stop("All vectors in `fix` must have the same length") }
  
  # Produce slices
  plots <- lapply(1:nsl, 
                  function(.ii){
                    .vr <- sapply(fix, "[[", .ii)
                    plot(o, fix = .vr, ...)
                  })
  
  out <- structure(list("plots" = plots, "fix" = fix, "noiseup" = noiseup), 
                   class="smooth.slice.2D")
  
  return( out )
}