
#' Generic check function
#'
#' Generic check function
#'
#' @title check
#' @examples
#' @rdname check
#' @export check
check <- function(x, ...) UseMethod("check")

#' Generic zooming function
#'
#' Generic zooming function
#'
#' @title zooming
#' @examples
#' @rdname zoom
#' @export zoom
zoom <- function(x, ...) UseMethod("zoom")

.prepare <- function(...) UseMethod(".prepare")

.plot <- function(...) UseMethod(".plot")