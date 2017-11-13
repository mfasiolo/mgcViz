#'
#' Add glyphs to 2D plot
#' 
#' @description XXX
#'
#' @param glyFun the function used to obtain the data needed to construct the glyphs.
#' @param ggLay the ggplot2 layer function used to plot the glyphs.
#' @param n vector of two positive integers, indicating the number of 2D grid cell in which
#'          the data is divided.
#' @param mapping list of aesthetic mappings to be used by \code{ggLay}. By default it is
#'                \code{aes(x=gx, y=gy, group = gid)}.
#' @param data a data.frame to be used for computing the glyphs. It must have two variables
#'             called `x` and `y`.
#' @param polar see \code{?GGally::glyphs}.
#' @param height see \code{?GGally::glyphs}.
#' @param width see \code{?GGally::glyphs}.
#' @param y_scale see \code{?GGally::glyphs}.
#' @param x_scale see \code{?GGally::glyphs}.
#' @param ... graphical arguments to be passed to \code{ggLay}.
#' @importFrom GGally glyphs
#' @return An object of class \code{gamLayer}.
#' @examples 
#' library(mgcViz);
#' set.seed(4124)
#' n <- 1e4
#' dat <- data.frame("x1" = rnorm(n), "x2" = rnorm(n))
#' 
#' # Residuals are heteroscedastic w.r.t. x1
#' dat$y <- (dat$x1)^2 + (dat$x2)^2 + (1*abs(dat$x1) + 1)  * rnorm(n)
#' b <- bam(y ~ s(x1,k=30) + s(x2, k=30), data = dat, discrete = TRUE)
#' 
#' pl <- check2D(b, x1 = "x1", x2 = "x2", type = "tnormal") + 
#'   l_points(colour = "blue", alpha = 0.5)
#' 
#' # Look at distributions of residuals across x1 and x2
#' # Approach 1: using binned kernel density estimate
#' # Colour indicates whether we have more that 50 obs in that bin
#' glyFun <- function(.d){
#'   .r <- .d$z
#'   .qq <- as.data.frame( density(.r)[c("x", "y")], n = 100 )
#'   .qq$colour <- rep(ifelse(length(.r)>50, "black", "red"), nrow(.qq))
#'   return( .qq )
#' }
#' 
#' pl + l_glyphs2D(glyFun = glyFun, ggLay = "geom_path", n = c(8, 8),
#'                  mapping = aes(x=gx, y=gy, group = gid, colour = I(colour)), 
#'                  height=1.5, width = 1) 
#' 
#' # Approach 2: using binned worm-plots. These are simply rotated QQplots.
#' # An horizontal plot indicates well specified residual model. 
#' # Increasing (decreasing) worm indicates over (under) dispersion
#' glyFun <- function(.d){
#'   n <- nrow(.d)
#'   px <- qnorm( (1:n - 0.5)/(n) )
#'   py <- sort( .d$z )
#'   clr <- if(n > 50) { "black" } else { "red" }
#'   clr <- rep(clr, n)
#'   return( data.frame("x" = px, "y" = py - px, "colour" = clr))
#' }
#' 
#' pl + l_glyphs2D(glyFun = glyFun, ggLay = "geom_point", n = c(10, 10),
#'                 mapping = aes(x=gx, y=gy, group = gid, colour = I(colour)),
#'                 height=2, width = 1, size = 0.2) 
#' 
#' @export l_glyphs2D
#'
l_glyphs2D <- function(glyFun, ggLay = "geom_points", n = c(4, 4), mapping = NULL,  
                        data = NULL, polar = FALSE, height = ggplot2::rel(0.95), 
                        width = ggplot2::rel(0.95), y_scale = I, x_scale = I, ...){
  arg <- list(...)
  arg$xtra <- list("glyFun" = glyFun, "ggLay" = ggLay, "n" = n, "mapping" = mapping,
                   "data" = data, "polar" = polar, "height" = height, "width" = width, 
                   "y_scale" = y_scale, "x_scale" = x_scale)
  
  o <- structure(list("fun" = "l_glyphs2D",
                      "arg" = arg), 
                 class = "gamLayer")
  return(o)
}

######## Internal method
#' @noRd
l_glyphs2D.Check2DNumericNumeric <- function(a){
  
  xtra <- a$xtra
  a$xtra <- NULL
  ggLay <- xtra$ggLay
  
  # Data used to compute the glyphs
  gldat <- a$extra$data
  if( is.null(gldat) ) { gldat <- a$data$res }
  
  # Compute data for glyphs
  gldat <- .getGlyData(dat = gldat, 
                       vx = "x", vy = "y", 
                       glyFun = xtra$glyFun, ngr = xtra$n)
  
  # Transform into a single data set
  a$data <- glyphs(data = gldat, x_major = "Xc", x_minor = "x", y_major = "Yc", y_minor = "y", 
                   polar = xtra$polar, height = xtra$height, width = xtra$width, 
                   y_scale = xtra$y_scale, x_scale = xtra$x_scale)
  
  # Add arguments for `geom_raster`
  a$mapping <- xtra$mapping
  if( is.null(a$mapping) ) { a$mapping <- aes(x=gx, y=gy, group = gid) }
  a$inherit.aes <- FALSE
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE }
  
  # Build layer
  out <- do.call(ggLay, a) 
  
  return( out )
  
}

