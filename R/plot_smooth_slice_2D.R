
#' Plotting two dimensional slices of a smooth effect
#' 
#' @description XXX
#' @name plot.smooth.slice.2D
#' @examples 
#' ## 3D example
#' library(mgcViz)
#' n <- 1e3
#' x <- rnorm(n); y <- rnorm(n); z <- rnorm(n)
#' 
#' ob <- (x-z)^2 + (y-z)^2 + rnorm(n)
#' b <- gam(ob ~ s(x, y, z))
#' v <- getViz(b)
#' 
#' # Plot a sequence of 2D slices with common scale
#' sm <- slice(v(1), fix = list("z" = seq(-2, 2, length.out = 8)), scheme = 2)
#' plot(sm)
#'
#' # No common scale
#' plot(sm, scaleCom = FALSE)
#' 
#' ## 4D
#' n <- 5e3
#' x <- rnorm(n); y <- rnorm(n); z <- rnorm(n); z2 <- rnorm(n)
#' 
#' ob <- (x-z)^2 + (y-z)^2 + z2^3 + rnorm(n)
#' b <- bam(ob ~ s(x, y, z, z2), discrete = T)
#' v <- getViz(b)
#' 
#' # Plot a sequence of 2D slices
#' sm <- slice(v(1), fix = list("z" = seq(-2, 2, length.out = 7), 
#'                              "x" = rep(0, 7)), 
#'             scheme = 2, rug = FALSE)
#' plot(sm)
#'
#' @rdname plot.smooth.slice.2D
#' @importFrom grid unit.c
#' @importFrom gridExtra arrangeGrob
#' @export plot.smooth.slice.2D
#' 
plot.smooth.slice.2D <- function(o, scaleCom = TRUE, scaleLim = NULL, ncol = NULL, labTsize = 10, 
                                titles = NULL, xlab = NULL, ylab = NULL, ...)
{
  
  # Inside
  plots <- o$plots
  nsl <- length(plots)
  if( is.null(ncol) ){ ncol <- ceiling(sqrt(nsl)) }
  if( is.null(xlab) ){ xlab <- plots[[1]]$labels$x }
  if( is.null(ylab) ){ ylab <- plots[[1]]$labels$y }
  
  # Set scale 
  if( !is.null(scaleLim) || scaleCom ){
    if( !is.null(scaleLim) ){ # User-defined scale for all plots
      ran <- scaleLim  
    } else{
      if( scaleCom ){ # Common scale
        ran <- do.call("range", lapply(plots, function(.inp) range(.inp$data$z, na.rm = TRUE)))
      }
    }
    # Change scale of all ggObjects
    for(ii in 1:nsl) plots[[ii]]$scales$scales[[1]]$limits <- ran
  }
  
  # Remove legends
  for(ii in 1:nsl){ 
    # Remove axes titles
    plots[[ii]] <- plots[[ii]] + theme(axis.title.x=element_blank(), 
                                       axis.title.y=element_blank(), 
                                       plot.title = element_text(size=labTsize))
    # Modify title of each plot
    plots[[ii]]$labels$title <- labs(title = 
                                       if(is.null(titles[[ii]])){
                                         tmp <- lapply(1:length(o$fix), function(kk){
                                           paste(names(o$fix)[[kk]], "=", round(o$fix[[kk]][ii], 3))
                                         } )
                                         element_text( as.character(do.call("paste", c(tmp, sep = ', '))) )
                                       } else {
                                         titles[[ii]]
                                       })
    # Remove scale legend
    if( scaleCom ){ plots[[ii]] <- plots[[ii]] + theme(legend.position="none") } 
  }
  
  toGrid <- list()
  toGrid[[1]] <- do.call("arrangeGrob", list("grobs" = plots, left = ylab, bottom = xlab, ...))
  
  if( scaleCom ){
    
    tmp <- ggplotGrob(plots[[1]] + theme(legend.position="right"))$grobs
    legend <- tmp[[which(sapply(tmp, function(x) x$name) == "guide-box")]]
    lwidth <- sum(legend$width)
    toGrid[[2]] <- legend
    toGrid$widths <- unit.c(unit(1, "npc") - lwidth, lwidth)
    toGrid$ncol <- 2
    
  }
  
  out <- do.call("grid.arrange", toGrid)
  
  return( invisible(out) )
  
}

