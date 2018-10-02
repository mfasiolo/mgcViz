
#'
#' Generic zooming function
#'
#' @description Generic function for zooming, mainly meant to work with graphical objects.
#' @param o the object we want to zoom into.
#' @param ... arguments to be passed to methods.
#' @seealso zoom.qqGam
#' @rdname zoom
#' @export zoom
zoom <- function(o, ...) UseMethod("zoom")

#'
#' Generic RGL plotting function
#'
#' @description Generic function for producing an interactive RGL plot.
#' @param x the object we want to plot using the \code{rgl} package.
#' @param ... arguments to be passed to methods.
#' @seealso [plotRGL.mgcv.smooth.2D], [plotRGL.mgcv.smooth.MD]
#' @rdname plotRGL
#' @export plotRGL
plotRGL <- function(x, ...) UseMethod("plotRGL")

#'
#' Generic plotting of differences 
#'
#' @description Generic function for plotting differences between objects. Useful
#'              mainly for plotting the differences between two smooth effects.
#' @param ... arguments to be passed to methods. This first one will determine which
#'            method will be called.
#' @seealso [plotDiff.mgcv.smooth.1D], [plotDiff.mgcv.smooth.2D], [plotDiff.sos.smooth]
#' @rdname plotDiff
#' @export plotDiff
plotDiff <- function(...) UseMethod("plotDiff")

#'
#' Generic QQ plots 
#'
#' @description Generic function for producing QQ-plots.
#' @param ... arguments to be passed to methods. This first one will determine which
#'            method will be called.
#' @seealso [qq.gamViz]
#' @rdname qq
#' @export qq
qq <- function(...) UseMethod("qq")

###### Internal generics
.prepare <- function(...) UseMethod(".prepare")

.plot <- function(...) UseMethod(".plot")