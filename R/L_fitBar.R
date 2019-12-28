#'
#' Adding barplot to effect plots
#' 
#' @description This layer adds a barplot to an effect plots. Mainly useful for factor or
#'              binary effect plots.
#'
#' @param a.aes list of aesthetic mapping arguments that will be passed to
#'              \code{ggplot2::geom_bar}. For instance we could set \code{a.aes=list("fill"="red")} 
#'              to change the colour of the barplot.
#' @param ... graphical arguments to be passed to [ggplot2::geom_bar].
#' @return an object of class \code{gamLayer}.
#' @seealso See [plot.ptermFactor] for examples.
#' @export l_fitBar
#'
l_fitBar <- function(a.aes = list(), ...){
  arg <- list(...)
  arg$xtra <- list("a.aes" = a.aes)
  o <- structure(list("fun" = "l_fitBar",
                      "arg" = arg), 
                 class = c("gamLayer"))
  return(o)
}


######## Internal method for factor plots
#' @noRd
#'
l_fitBar.PtermFactor <- l_fitBar.MultiPtermNumeric <- l_fitBar.MultiPtermFactor <- l_fitBar.ALE1DFactor <- function(a){
  
  a$data <- a$data$fit
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE}
  
  a.aes <- a$xtra$a.aes
  a$xtra <- NULL
  
  a.aes$x <- a.aes$y <- NULL
  if( is.null(a.aes$fill) ) a.aes$fill <- I("light grey")
  
  a$mapping <- do.call("aes", a.aes)
  a$stat <- "identity"
  out <- do.call("geom_bar", a)
  return( out )
  
}