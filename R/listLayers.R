#'
#' Lists available layers for plotSmooth objects
#'
#' @description This function takes as input an object of class \code{plotSmooth} and
#'              returns the names of all the possible visual layers that
#'              could be used with that object.
#'
#' @param o an object of class \code{plotSmooth}.
#' @return A vector containing the names of the available layers.
#' @examples 
#' library(mgcViz)
#' n  <- 400
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#' dat <- data.frame("x1" = x1, "x2" = x2,
#'                   "y" = sin(x1) + 0.5 * x2^2 + rnorm(n))
#' b <- gam(y ~ x1+s(x2), data = dat, method = "REML")
#' b <- getViz(b)
#' 
#' # List layers available for parametric effect plot
#' o <- plot( pterm(b, 1) )
#' listLayers(o)
#' 
#' # List layers available for smooth effect plot
#' o <- plot( sm(b, 1) )
#' listLayers(o)
#' 
#' # List layers available for checking plot
#' o <- check1D(b, x1)
#' listLayers(o)
#'
#' @rdname listLayers
#' @export listLayers
#' 
listLayers <- function(o){

  if( !("plotSmooth" %in% class(o)) ){
    stop("listLayers works only with objects of class \"plotSmooth\"")
  }
  
  ty <- paste(o$type, collapse = '')
  
  out <- switch(ty, 
                # Smooth effects plots
                "fs1D" = c("l_fitLine"),
                "1D" = c("l_ciLine", "l_ciPoly", "l_fitLine", "l_dens", "l_points", "l_rug"),
                "Multi1D" = c("l_fitLine", "l_rug"),
                "2D" = c("l_fitContour", "l_fitRaster", "l_dens", "l_points", "l_rug"), 
                "MD" = c("l_fitContour", "l_fitRaster", "l_dens", "l_points", "l_rug"), 
                "sos0" = c("l_fitContour", "l_fitRaster", "l_points", "l_rug", 
                                       "l_coordContour", "l_bound"), 
                "sos1" = c("l_fitContour", "l_fitRaster", "l_points", "l_rug"),
                "randomEffect" = c("l_fitLine", "l_points", "l_ciLine", "l_ciPoly"),
                "MultiRandomEffect" = c("l_fitLine", "l_points", "l_ciLine", "l_ciPoly"),
                "mrf" = c("l_poly"),
                
                # Slices of MD smooths
                "MDslice" = c("l_fitContour", "l_fitRaster", "l_points", "l_rug"),
                
                # Parametric effects plots
                "PtermNumeric" = c("l_ciLine", "l_ciPoly", "l_fitLine", "l_dens", 
                                               "l_points", "l_rug"), 
                "MultiPtermNumeric" = c("l_fitPoints", "l_fitBar", "l_ciBar"), 
                "PtermFactor"  =  c("l_fitPoints", "l_fitBar", "l_ciBar", "l_points", "l_rug"),
                "MultiPtermFactor"  =  c("l_fitPoints", "l_ciBar"),
                "PtermLogical" = c("l_fitPoints", "l_fitBar", "l_ciBar", "l_points", "l_rug"),
                
                # Checking plots
                "Check1DNumeric" = c("l_densCheck", "l_gridCheck1D", "l_gridQCheck1D", "l_dens", "l_points", "l_rug"),
                "Check1DFactor" = c("l_gridCheck1D", "l_gridQCheck1D", "l_points", "l_rug"),
                "Check1DLogical" = c("l_gridCheck1D", "l_gridQCheck1D", "l_points", "l_rug"),
                "Check2DNumericNumeric" = c("l_gridCheck2D", "l_dens", "l_glyphs2D", "l_points", "l_rug"),
                "Check2DFactorNumeric" = c("l_gridCheck2D", "l_points", "l_rug"),
                "Check2DFactorFactor" = c("l_gridCheck2D", "l_points", "l_rug"),

                message(paste("No layers available for objects of type", paste(ty, collapse = ' '))))
  
  return(out)

} 