
## Internal function used by check0D, check1D and check2D
#
.getDataTypeY <- function(o, type){
  
  o$y <- as.matrix(o$y)
  d <- ncol(o$y)
  
  if( !is.null(o$store$newdata) ){ # (1) Newdata has been provided, so this is a predictive check OR ...
    ynam <- if(is.list(o$formula)){ lapply(1:d, function(ii) o$formula[[ii]][[2]]) } else { list(o$formula[[2]]) }
    data <- o$store$newdata
    y <- sapply(ynam, function(nm) data[[nm]])
    if(type == "auto") { type <- "y" }
    if(type != "y") { 
      stop("Predictive checks on newdata can be performed only with raw observations (type == \"y\"). See ?check0D") 
    }
  } else { # ... (2) No newdata so we get either residuals or responses y
    data <- o$model
    if(type == "y"){
      y <- o$y
    } else {
      # Returns the appropriate residual type for each GAM family
      if( type=="auto" ) { type <- .getResTypeAndMethod(o$family)$type }
      y <- residuals(o, type = type)
    }
  }
  
  return( list("y" = y, "data" = data, "type" = type) )
  
}