#'
#' Lists available layer
#'
#' @param o an object.
#' @rdname listLayers
#' @export listLayers
#' 
listLayers <- function(o){

  if( !("plotSmooth" %in% class(o)) ){
    stop("listLayers works only with object of class \"plotSmooth\"")
  }
  
  ty <- paste(o$type, collapse = '')
  
  out <- switch(ty, 
                # Smooth effects plots
                "fs1D" = c("l_fitLine"),
                "1D" = c("l_ciLine", "l_ciPoly", "l_fitLine", "l_dens", "l_points", "l_rug"),
                "2D" = c("l_fitContour", "l_fitRaster", "l_dens", "l_points", "l_rug"), 
                "MD" = c("l_fitContour", "l_fitRaster", "l_dens", "l_points", "l_rug"), 
                "sos0" = c("l_fitContour", "l_fitRaster", "l_points", "l_rug", 
                                       "l_coordContour", "l_bound"), 
                "sos1" = c("l_fitContour", "l_fitRaster", "l_points", "l_rug"),
                "randomEffect" = c("l_fitLine", "l_points", "l_ciLine", "l_ciPoly"),
                "mrf" = c("l_poly"),
                
                # Slices of MD smooths
                "MDslice" = c("l_fitContour", "l_fitRaster", "l_points", "l_rug"),
                
                # Parametric effects plots
                "PtermNumeric" = c("l_ciLine", "l_ciPoly", "l_fitLine", "l_dens", 
                                               "l_points", "l_rug"), 
                "PtermFactor"  = c("l_fitPoints", "l_fitBar", "l_ciBar", "l_points", "l_rug"),
                "PtermLogical" = c("l_fitPoints", "l_fitBar", "l_ciBar", "l_points", "l_rug"),
                
                # Checking plots
                "Check1DNumeric" = c("l_densCheck", "l_gridCheck1D", "l_dens", "l_points", "l_rug"),
                "Check1DFactor" = c("l_gridCheck1D", "l_points", "l_rug"),
                "Check1DLogical" = c("l_gridCheck1D", "l_points", "l_rug"),
                "Check2DNumericNumeric" = c("l_gridCheck2D", "l_dens", "l_glyphs2D", "l_points", "l_rug"),
                "Check2DFactorNumeric" = c("l_gridCheck2D", "l_points", "l_rug"),
                "Check2DFactorFactor" = c("l_gridCheck2D", "l_points", "l_rug"),

                message(paste("No layers available for objects of type", paste(ty, collapse = ' '))))
  
  return(out)

} 