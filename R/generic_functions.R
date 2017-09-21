
#' Generic check function
#'
#' Generic check function
#'
#' @title check
#' @rdname check
#' @export check
check <- function(x, ...) UseMethod("check")

#' Generic zooming function
#'
#' Generic zooming function
#'
#' @title zooming
#' @rdname zoom
#' @export zoom
zoom <- function(x, ...) UseMethod("zoom")

.prepare <- function(...) UseMethod(".prepare")

.plot <- function(...) UseMethod(".plot")

#' @export
shine <- function(...) UseMethod("shine")

#' @export
slice <- function(...) UseMethod("slice")

#' @export
plotRGL <- function(...) UseMethod("plotRGL")