
#' Generic zooming function
#'
#' Generic zooming function
#'
#' @title zooming
#' @rdname zoom
#' @export zoom
zoom <- function(o, ...) UseMethod("zoom")

#' @export
shine <- function(...) UseMethod("shine")

#' @export
plotRGL <- function(...) UseMethod("plotRGL")

#' @export
plotDiff <- function(...) UseMethod("plotDiff")

###### Internal generics
.prepare <- function(...) UseMethod(".prepare")

.plot <- function(...) UseMethod(".plot")