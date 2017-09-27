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
                "plotSmoothfs1D" = c("fitLine"),
                "plotSmooth1D" = c("ciLine", "ciPoly", "fitLine", "resDens", "resPoints", "resRug"),
                "plotSmooth2D" = c("fitLine", "fitRaster", "resDens", "resPoints", "resRug"), 
                "plotSmoothMD" = c("fitLine", "fitRaster", "resDens", "resPoints", "resRug"), 
                
                "plotSmoothCheck1D" = c("denCheck", "gridCheck1D", "resDens", "resPoints", "resRug"), 
                "plotSmoothCheck1D" = c("gridCheck2D", "resDens", "resGlyphs2D", "resPoints", "resRug"),
                
                message(paste("No layers for obj of this class", paste(class(o), collapse = ' '))))
  
  return(out)

} 