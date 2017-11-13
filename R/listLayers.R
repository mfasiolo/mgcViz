#'
#' Lists available layer
#'
#' @param o an object.
#' @rdname listLayers
#' @export listLayers
#' 
listLayers <- function(o){
  
  cl <- paste(class(o), collapse = '')
  
  out <- switch(cl, 
                # Smooth effects plots
                "plotSmoothfs1Dgg" = c("l_fitLine"),
                "plotSmooth1Dgg" = c("l_ciLine", "l_ciPoly", "l_fitLine", "l_dens", "l_points", "l_rug"),
                "plotSmooth2Dgg" = c("l_fitContour", "l_fitRaster", "l_dens", "l_points", "l_rug"), 
                "plotSmoothMDgg" = c("l_fitContour", "l_fitRaster", "l_dens", "l_points", "l_rug"), 
                "plotSmoothsos0gg" = c("l_fitContour", "l_fitRaster", "l_points", "l_rug", 
                                       "l_coordContour", "l_bound"), 
                "plotSmoothsos1gg" = c("l_fitContour", "l_fitRaster", "l_points", "l_rug"),
                "plotSmoothrandomEffectgg" = c("l_fitLine", "l_points", "l_ciLine", "l_ciPoly"),
                "plotSmoothmrfgg" = c("l_poly"),
                
                # Slices of MD smooths
                "plotSmoothMDslicegg" = c("l_fitContour", "l_fitRaster", "l_points", "l_rug"),
                
                # Parametric effects plots
                "plotSmoothPtermNumericgg" = c("l_ciLine", "l_ciPoly", "l_fitLine", "l_dens", 
                                               "l_points", "l_rug"), 
                "plotSmoothPtermFactorgg"  = c("l_fitPoints", "l_fitBar", "l_ciBar", "l_points", "l_rug"),
                "plotSmoothPtermLogicalgg" = c("l_fitPoints", "l_fitBar", "l_ciBar", "l_points", "l_rug"),
                
                # Checking plots
                "plotSmoothCheck1DNumericgg" = c("l_densCheck", "l_gridCheck1D", "l_dens", "l_points", "l_rug"),
                "plotSmoothCheck1DFactorgg" = c("l_gridCheck1D", "l_points", "l_rug"),
                "plotSmoothCheck1DLogicalgg" = c("l_gridCheck1D", "l_points", "l_rug"),
                "plotSmoothCheck2DNumericNumericgg" = c("l_gridCheck2D", "l_dens", "l_glyphs2D", "l_points", "l_rug"),
                "plotSmoothCheck2DFactorNumericgg" = c("l_gridCheck2D", "l_points", "l_rug"),
                "plotSmoothCheck2DFactorFactorgg" = c("l_gridCheck2D", "l_points", "l_rug"),

                message(paste("No layers for obj of this class", paste(class(o), collapse = ' '))))
  
  return(out)

} 