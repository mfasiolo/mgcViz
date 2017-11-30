#'
#' Cluster and plot smooth effects
#' 
#' @description This layers clusters several smooth effects and plots the cluster centers.
#'
#' @param centers the number of clusters. This is the same a the \code{centers} 
#'                argument in [stats::kmeans].
#' @param cluFun the function used for clustering. I must take (at least) arguments \code{x}, 
#'               \code{centers} and \code{data}, which have the same interpretation as in
#'               [stats::kmeans] (which is the default).
#' @param a.clu list of further argument to be passed to \code{cluFun}.
#' @param ... graphical arguments to be passed to \code{ggplot2::geom_line}.
#' @return An object of class \code{gamLayer}.
#' @seealso See [plot.fs.interaction.1D] for examples.
#' @importFrom stats kmeans
#' @export l_clusterLine
#'
l_clusterLine <- function(centers, cluFun = kmeans, a.clu = list(), ...){
  
  a.clu$centers <- centers
  arg <- list(...)
  arg$xtra <- list("cluFun" = cluFun, "a.clu" = a.clu)
  o <- structure(list("fun" = "l_clusterLine",
                      "arg" = arg), 
                 class = c("gamLayer"))
  return(o)
  
}


######## Internal method 
#' @noRd
l_clusterLine.fs1D <- function(a){
  
  .a.clu <- a$xtra$a.clu
  .cluFun <- a$xtra$cluFun
  a$xtra <- NULL
  
  ## 1)  Cluster the curves using .cluFun()
  .dat <- a$data$fit
  .n <- sum( .dat$id == .dat$id[1] )
  .nlev <- nlevels( .dat$id )
  .a.clu$x <- matrix(a$data$fit$ty, .nlev, .n, byrow = TRUE)
  .cl <- do.call(".cluFun", .a.clu)
  .nuc <- nrow( .cl$centers )
  
  ## 2) Create data set for geom_line()
  a$data <- data.frame("x" = rep(.dat$x[1:.n], .nuc), 
                       "ty" = as.vector( t(.cl$centers) ), 
                       "id" = as.factor(rep(1:.nuc, each = .n)))
  if( is.null(a$na.rm) ){ a$na.rm <- TRUE}

  ## 3) Create plot
  out <- do.call("geom_line", a)
  
  return( out )
  
}

