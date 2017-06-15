#' Trasparency level as function of p value
#' 
#' @description XXX
#' @name zto1Creator
#' @examples 
#' library(mgcViz)
#' x <- seq(0, 1, by = 0.01)
#' plot(x, zto1Creator(0.05, 2)(x))
#' lines(x, zto1Creator(0.05, 1)(x), col = 2)
#' lines(x, zto1Creator(0.1, 3)(x), col = 3)
#' @rdname zto1Creator
#' @export zto1Creator
zto1Creator <- function(o, a){
  
  .f <- function(.p){ 
    
    .p<-pmax(0, .p-o) 
    return( (1-.p)^a ) 
    
    }
  
  return(.f)
}

#' Extract specific smooth from gamObject
#' 
#' @description XXX
#' @name getSmooth
#' @examples 
#' library(mgcViz)
#' @rdname getSmooth
#' @export getSmooth
getSmooth <- function(x, select, fit=NULL){
  
  m <- length(x$smooth) # number of smooth effects

  if(length(select)>1){ stop("select should be a scale") }
  if(select > m){ stop(paste("select should be smaller than", m, "the number of smooths in x")) }
  
  if(is.null(fit)){
    np <- if (is.list(x$pterms)){ length(unlist(lapply(x$pterms,attr,"order"))) } else { length(attr(x$pterms,"order")) }
    x$fit <- predict(x, type = "terms")[ , np + select]
  } else {
    x$fit <- fit
  }
  
  x$smooth <- x$smooth[[select]]
  x <- x[ c("smooth", "edf", "coefficients", "residuals", "weights", "model", "cmX", "Vp", "Vc", "pterms", "fit") ]
  x$ism <- select
  
  cl <- class(x$smooth)
  
  if("mgcv.smooth" %in% cl){
    cl[which(cl=="mgcv.smooth")] <- paste("mgcv.smooth.", x$smooth$dim, "D", sep='')  
    class(x) <- cl
  }
  
  return( x )
  
}





## Local function for producing labels
.subEDF <- function(lab,edf) {
  ## local function to substitute edf into brackets of label
  ## labels are e.g. smooth[[1]]$label
  pos <- regexpr(":", lab)[1]
  if (pos<0) { ## there is no by variable stuff
    pos <- nchar(lab) - 1
    lab <- paste(substr(lab, start=1, stop=pos),", ", round(edf, digits=2),")",sep="")
  } else {
    lab1 <- substr(lab, start=1, stop=pos-2)
    lab2 <- substr(lab, start=pos-1, stop=nchar(lab))
    lab <- paste(lab1, ",", round(edf,digits=2), lab2, sep="")
  }
  lab
} ## end of sub.edf
