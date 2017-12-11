#'
#' Adding glyphs to 2D plots
#' 
#' @description This layer adds glyphs or subplots to 2D plots. It is mainly meant to 
#'              be used with [check2D] and to produce residuals checks.
#'
#' @param glyFun the function that produces the data needed to construct the glyphs.
#'               It will take a single argument (\code{.d}), which is a \code{data.frame}
#'               with columns \code{"x"}, \code{"y"} and \code{"z"}. When \code{l_glyphs2D}
#'               is used with \code{check2D}, then \code{"x"} and \code{"y"} will be the 
#'               locations of the residual \code{"z"} in the relevant covariates. 
#'               \code{glyFun} needs to output a \code{data.frame} that will be passed to
#'               the \code{ggLay} function, which does the plotting.
#' @param ggLay the \code{ggplot2} layer function (such as \code{"geom_point"}) used to 
#'              plot the glyphs. Its mapping needs to take at least argument "x", "y" and
#'              "group". See the \code{mapping} argument below.
#' @param n vector of two positive integers, indicating the number of 2D grid cell 
#'          along x and y in which the data is divided.
#' @param mapping list of aesthetic mappings to be used by \code{ggLay}. By default it is
#'                \code{aes(x=gx, y=gy, group = gid)}. Here gx and gy specify the x-y 
#'                location of each data-point used to plot the glyphs, while gid specifies
#'                to which glyph each data-point belongs (there are \code{n[1]*n[2]} glyphs).
#' @param data an optional data.frame to be used for computing the glyphs. 
#'             It must have two variables called `x` and `y`. If left to \code{NULL} then 
#'             the glyphs will be computed using the data in the \code{plotSmooth} object
#'             to which this layer is being added.
#' @param polar,height,width,y_scale,x_scale see [GGally::glyphs].
#' @param ... graphical arguments to be passed to \code{ggLay} function.
#' @importFrom GGally glyphs
#' @return An object of class \code{gamLayer}.
#' @seealso [check2D].
#' @examples 
#' library(mgcViz);
#' set.seed(4124)
#' n <- 1e4
#' dat <- data.frame("x1" = rnorm(n), "x2" = rnorm(n))
#' 
#' # Residuals are heteroscedastic w.r.t. x1
#' dat$y <- (dat$x1)^2 + (dat$x2)^2 + (1*abs(dat$x1) + 1)  * rnorm(n)
#' b <- bam(y ~ s(x1,k=30) + s(x2, k=30), data = dat, discrete = TRUE)
#' b <- getViz(b)
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

