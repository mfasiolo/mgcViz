
#' Generic check function
#'
#' Generic check function
#'
#' @title check
#' @examples
#' @rdname check
#' @export check
check <- function(x, ...) UseMethod("check")

.prepare <- function(...) UseMethod(".prepare")

.plot <- function(...) UseMethod(".plot")