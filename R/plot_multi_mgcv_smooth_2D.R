
#' @rdname plot.mgcv.smooth.2D
#' @export plot.multi.mgcv.smooth.2D
#' @export
#' 
plot.multi.mgcv.smooth.2D <- function(x, n = 30, xlim = NULL, ylim = NULL, maxpo = 1e4, 
                                      too.far = 0.1, trans = identity, seWithMean = FALSE, 
                                      unconditional = FALSE, a.facet = list(), ...)  {
  
  # 1) Prepare data
  tmp <- lapply(x, function(.inp) plot(.inp, n = n, xlim = xlim, ylim = ylim, maxpo = maxpo, 
                                       too.far = too.far, trans = trans, seWithMean = seWithMean, 
                                       unconditional = unconditional, ...))
  
  P <- list("data" = lapply(tmp, "[[", "data"), 
            "main" = paste0("s(",tmp[[1]]$ggObj$labels$x, ", ", tmp[[1]]$ggObj$labels$y, ")"), 
            "xlab" = tmp[[1]]$ggObj$labels$x,
            "ylab" = tmp[[1]]$ggObj$labels$y, 
            "misc" = tmp[[1]]$data$misc)
  names( P$data ) <- names( x )
  
  # 2) Produce output object
  out <- .plot.multi.mgcv.smooth.2D(P = P, trans = trans, a.facet = a.facet)
  
  class(out) <- c("plotSmooth", "gg")
  
  return(out)
}

############### Internal function
#' @noRd
.plot.multi.mgcv.smooth.2D <- function(P, trans, a.facet){
  
  n <- nrow(P$data[[1]]$fit)
  nr <- nrow(P$data[[1]]$res)
  nsl <- length( P$data )
  gridVar <- "qu"
  
  # Get data for quantile
  for(ii in 1:nsl){
    tmp <- paste0("qu=", names(P$data)[ii])
    P$data[[ii]]$fit$.fx.qu <- rep(tmp, n)
    P$data[[ii]]$res$.fx.qu <- rep(tmp, nr)
    tmp <- as.numeric(names(P$data)[ii]) 
    P$data[[ii]]$fit$qu <- as.factor(rep(tmp, n))
    P$data[[ii]]$res$qu <- as.factor(rep(tmp, nr))
  }
  
  .dat <- list()
  .dat$fit <- do.call("rbind", lapply(P$data, function(.x) .x$fit))
  .dat$res <- do.call("rbind", lapply(P$data, function(.x) .x$res))
  .dat$misc <- P$misc
  
  .pl <- ggplot(data = .dat$fit, aes(x = x, y = y, z = tz)) +
    labs(title = P$title, x = P$xlab, y = P$ylab) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  if( is.null(a.facet$nrow) && is.null(a.facet$ncol) ){ a.facet$ncol <- floor(sqrt(nsl)) }
  if( is.null(a.facet$facets) ){ a.facet$facets <- as.formula(paste0("~ .fx.", gridVar)) }
  .pl <- .pl + do.call("facet_wrap", a.facet)
  
  .pl <- .pl + theme(panel.spacing = unit(0, "lines"))
  
  out <- structure(list("ggObj" = .pl, "data" = .dat, "type" = c("MD", "slice")), 
                   class = c("plotSmooth", "gg")) 
  
}
