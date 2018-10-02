#'
#' Quantile-Quantile Plots
#' 
#' @description This is a re-write of the QQ-plotting functions provided by \code{stats}, using the
#'              \code{ggplot2} library.
#'              `qqnorm` is a generic function the default method of which produces a normal 
#'              QQ plot of the values in y. `qqline` adds a line to a “theoretical”, by default normal,
#'              quantile-quantile plot which passes through the `probs` quantiles, by default the 
#'              first and third quartiles. `qqplot` produces a QQ plot of two datasets.
#' @param x, The first sample for `qqplot`.
#' @param y, The second or only data sample.
#' @param main,xlab,ylab, Plot labels. The xlab and ylab refer to the y and x axes 
#'                        respectively if datax = TRUE.
#' @param datax, Logical. Should data values be on the x-axis ?
#' @param distribution quantile function for reference theoretical distribution.
#' @param probs numeric vector of length two, representing probabilities. 
#'              Corresponding quantile pairs define the line drawn.
#' @param qtype the type of quantile computation used in [quantile].
#' @param ylim,..., Graphical parameters.
#' @import ggplot2
#' @note Help file is mainly from `stats::qqnorm` since this is a rewrite of `stats::qqplot`, 
#'       `stats::qqline` and `stats::qqnorm` using the ggplot2 library.
#' @name qqplots
#' @importFrom stats ppoints runif
#' @examples 
#' library(mgcViz)
#' y <- rt(500, df = 5)
#' 
#' # Compare new and old version of qqnorm
#' stats::qqnorm(y)
#' qqnorm(y)
#' 
#' # Compare new and old version of qqplot
#' x <- rt(200, df = 5)
#' y <- rt(300, df = 5)
#' stats::qqplot(x, y)
#' qqplot(x, y)
#' # add a qqline()
#' ggplot2::last_plot() + qqline(y = rt(500, df = 4.8), col = "green") 
#' 
#' ## "QQ-Chisquare" : --------------------------
#' y <- rchisq(500, df = 3)
#' ## Q-Q plot for Chi^2 data against true theoretical distribution:
#' x <- qchisq(ppoints(500), df = 3)
#' stats::qqplot(qchisq(ppoints(500), df = 3), rchisq(500, df = 3),
#'       main = expression("Q-Q plot for" ~~ {chi^2}[nu == 3]))
#' qqplot(qchisq(ppoints(500), df = 3), rchisq(500, df = 3),
#'       main = expression("Q-Q plot for" ~~ {chi^2}[nu == 3])) + theme_bw()
#' @rdname qqplots
#' @export
qqnorm <- function(y, ylim,
                   main = "Normal Q-Q Plot",
                   xlab = "Theoretical Quantiles", 
                   ylab = "Sample Quantiles",
                   datax = FALSE) {
  if (has.na <- any(ina <- is.na(y))) {
    yN <- y
    y <- y[!ina]
  }
  if (0 == (n <- length(y))) 
    stop("y is empty or has only NAs")
  x <- qnorm(ppoints(n))[order(order(y))]
  if (has.na) {
    y <- x
    x <- yN
    x[!ina] <- y
    y <- yN
  }
  xtitle <- ifelse(datax, ylab, xlab)
  ytitle <- ifelse(datax, xlab, ylab)
  data <- data.frame(x = x, y = y)
  if (missing(ylim)) {
    if (datax) {
      ylim <- range(x)
    } else {
      ylim <- range(y)
    }
  }
  p <- 
    ggplot2::ggplot(data) +
    ggplot2::ylim(ylim) + ggplot2::labs(title = main, x = xtitle, y = ytitle)
  if (datax) {
    p <- p + ggplot2::geom_point(aes(x = y, y = x))
  } else {
    p <- p + ggplot2::geom_point(aes(x = x, y = y))
  }
  return(p + theme_bw())
}

#' @rdname qqplots
#' @export
qqplot <- function(x, y,
                   xlab = deparse(substitute(x)), 
                   ylab = deparse(substitute(y)),
                   main = "Q-Q Plot"){
  sx <- sort(x)
  sy <- sort(y)
  lenx <- length(sx)
  leny <- length(sy)
  if (leny < lenx) 
    sx <- approx(1L:lenx, sx, n = leny)$y
  if (leny > lenx) 
    sy <- approx(1L:leny, sy, n = lenx)$y
  data <- data.frame(sx = sx, sy = sy)
  p <- 
    ggplot2::ggplot(data, ggplot2::aes(x = sx, y = sy)) +
    ggplot2::geom_point() + 
    ggplot2::labs(title = main, x = xlab, y = ylab)
  return(p + theme_bw())
}


#' @rdname qqplots
#' @export
qqline <- function(y, datax = FALSE, distribution = qnorm,
                   probs = c(0.25, 0.75), qtype = 7, ...) {
  
  stopifnot(length(probs) == 2, is.function(distribution))

  y <- quantile(y, probs, names = FALSE, type = qtype, na.rm = TRUE)
  x <- distribution(probs)
  if (datax) {
    slope <- diff(x)/diff(y)
    int <- x[1L] - slope * y[1L]
  }
  else {
    slope <- diff(y)/diff(x)
    int <- y[1L] - slope * x[1L]
  }
  
  arg <- list(...)
  arg$intercept <- int
  arg$slope <- slope
  
  out <- do.call("geom_abline", arg)
  
  return( out )

}