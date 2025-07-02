#'
#' Plotting sequence of slices of 2D smooth effect
#' 
#' @description This function allows to slice a multi-dimensional (D > 2) smooth effect, 
#'              and to plot the resulting sequence of 2D slices in an array of plots.
#'              
#' @param x a smooth effect object, extracted using [mgcViz::sm].
#' @param fix a named list of vectors, where the i-th entry of each vector indicates the value we want to 
#'            use for the covariate for i-th slice. When plotting a smooth in (d+2) dimensions, 
#'            we need d vectors, because d variables must be fixed. All vectors must have either the same length
#'            (the number of slices) or length 1. \code{fix} can contain at most 2 vectors, so if d>=5, we need to set
#'            at least one covariate to a scalar.
#' @param a.facet arguments to be passed to [ggplot2::facet_wrap] or [ggplot2::facet_grid]. The former gets
#'                called when \code{fix} contains one vector, the latter when \code{fix} contains two vectors.
#' @param ... further arguments to be passed to [plot.mgcv.smooth.MD].
#' @return An objects of class \code{plotSmooth}.
#' @name plotSlice
#' @examples 
#' \dontrun{
#' ### Example 1: plotting slices of 3D smooth
#' # Simulate data and fit GAM
#' library(mgcViz)
#' n <- 1e3
#' x <- rnorm(n); y <- rnorm(n); z <- rnorm(n)
#' ob <- (x-z)^2 + (y-z)^2 + rnorm(n)
#' b <- gam(ob ~ s(x, y, z))
#' v <- getViz(b)
#' 
#' # Get plot of slices and add layers
#' pl <- plotSlice(x = sm(v, 1), 
#'                 fix = list("z" = seq(-2, 2, length.out = 9)))
#' pl + l_fitRaster() + l_fitContour() + l_points() + l_rug()
#' 
#' # Over-ride default layout
#' pl <- plotSlice(x = sm(v, 1), 
#'                 fix = list("z" = seq(-2, 2, length.out = 9)), 
#'                 a.facet = list(nrow = 2))
#' pl + l_fitRaster() + l_fitContour() + theme(panel.spacing = unit(0.5, "lines"))
#' 
#' ### Example 2: plotting slices of 4D smooth
#' # Simulate data and fit GAM
#' n <- 5e3
#' x <- rnorm(n); y <- rnorm(n); z <- rnorm(n); z2 <- rnorm(n)
#' ob <- (x-z)^2 + (y-z)^2 + z2^3 + rnorm(n)
#' b <- bam(ob ~ s(x, y, z, z2), discrete = TRUE)
#' v <- getViz(b)
#' 
#' # Plot slices across "z" and "x"
#' pl <- plotSlice(x = sm(v, 1), 
#'                 fix = list("z" = seq(-2, 2, length.out = 3), "x" = c(-1, 0, 1)))
#' pl + l_fitRaster() + l_fitContour() + l_points() + l_rug()
#' 
#' # Plot slices across "x", keeping "z" fixed
#' pl <- plotSlice(x = sm(v, 1), 
#'                 fix = list("z" = 0, "x" = seq(-3, 3, length.out = 9)))
#' pl + l_fitRaster() + l_fitContour() + l_points() + l_rug()
#' }
#' 
#' @importFrom plyr alply
#' @importFrom stats as.formula
#' @rdname plotSlice
#' @export plotSlice
#' 
plotSlice <- function(x, fix, a.facet = list(), ...){
  
  if( !("mgcv.smooth.MD" %in% class(x)) ){
    stop( "x must be of class \"mgcv.smooth.MD\"" )
  }
  
  len <- sapply(fix, length)
  grD <- sum(len > 1)
  if( grD > 2 ) { 
    stop("'fix' cannot contain more than 2 vectors. Some variables must be fixed to scalars.") 
  }
  if( grD == 0){
    stop("'fix' does not contain any vector: cannot construct a grid of plots.")
  }
  
  gridVar <- names(fix)[ len > 1 ]
  nfx <- length(fix)
  
  # Create grid with all combinations of fixed variables. We'll get a slice of each.
  indx  <- as.matrix( do.call("expand.grid", fix)  )
  nsl <- nrow(indx)
  
  # Get data for each slice
  plts <- alply(indx, 1,
                function(.vr, ...){
                  .d <- plot(x, fix = .vr, ...)$data 
                  .d$fit[paste0(".fx.", names(.vr))] <- drop(matrix(rep(.vr, each = nrow(.d$fit)), 
                                                                    nrow(.d$fit), nfx))
                  .d$res[paste0(".fx.", names(.vr))] <- drop(matrix(rep(.vr, each = nrow(.d$res)), 
                                                                    nrow(.d$res), nfx))
                  return(.d)
                }, ...) 
  
  .dat <- list()
  .dat$fit <- do.call("rbind", lapply(plts, function(.x) .x$fit))
  .dat$res <- do.call("rbind", lapply(plts, function(.x) .x$res))
  .dat$misc <- plts[[1]]$misc
  
  # One extra plot just to get the labels
  lbs <- plot(x, fix = indx[1, ])$ggObj$labels
  
  .pl <- ggplot(data = .dat$fit, aes(x = x, y = y, z = tz)) +
    labs(title = lbs$title, x = lbs$x, y = lbs$y) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  if( is.null(a.facet$labeller) ){ a.facet$labeller <- function (labels, multi_line = TRUE, sep = "=") {
                                                          .labels <- label_both(labels, multi_line = TRUE, sep)
                                                          .labels <- lapply(.labels, function(x) substring(x ,5))  # Drop .fx. prefix from labels
                                                          return(.labels)
                                                       }
  }
  
  if( grD == 1 ){
    if( is.null(a.facet$nrow) && is.null(a.facet$ncol) ){ a.facet$ncol <- floor(sqrt(nsl)) }
    if( is.null(a.facet$facets) ){ a.facet$facets <- as.formula(paste0("~ .fx.", gridVar)) }
    .pl <- .pl + do.call("facet_wrap", a.facet)
  }
  if( grD == 2 ){
    if( is.null(a.facet$rows) ){ a.facet$rows <- paste0(".fx.", gridVar[1], " ~ .fx.", gridVar[2]) }
    .pl <- .pl + do.call("facet_grid", a.facet)
  }
  
  .pl <- .pl + theme(panel.spacing = unit(0, "lines"))
  
  out <- structure(list("ggObj" = .pl, "data" = .dat, "type" = c("MD", "slice")), 
                   class = c("plotSmooth", "gg")) 
  
  return( out )
  
}

