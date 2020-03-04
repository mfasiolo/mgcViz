
#' @rdname plot.ptermFactor
#' @export plot.multi.ptermFactor
#' @export
#' 
plot.multi.ptermFactor <- function(x, a.facet = list(), asFact = TRUE, ...) {
  
  if(x[[1]]$order > 1){ 
    message("mgcViz does not know how to plot this effect. Returning NULL.")
    return( invisible(NULL) ) 
  }
  
  # 1) Prepare data
  tmp <- lapply(x, function(.inp) plot(.inp, ...))
  
  P <- list("data" = lapply(tmp, "[[", "data"), 
            "main" = tmp[[1]]$ggObj$labels$title, 
            "xlab" = tmp[[1]]$ggObj$labels$x,
            "ylab" = tmp[[1]]$ggObj$labels$y)
  names( P$data ) <- names( x )
  
  # 2) Produce output object
  out <- .plot.multi.ptermFactor(P = P, trans = identity, a.facet = a.facet, 
                                 asFact = asFact, isMQGAM =  attr(x, "isMQGAM"))
  
  class(out) <- c("plotSmooth", "gg")
  
  return(out)
}

############### Internal function
#' @noRd
.plot.multi.ptermFactor <- function(P, trans, a.facet, asFact, isMQGAM){
  
  .fitDat <- lapply(P$data, "[[", "fit")
  
  basel <- .fitDat[[1]]$x[.fitDat[[1]]$y == 0]
  
  suppressWarnings( .idNam <- as.numeric(names(P$data)) )
  if( anyNA(.idNam) ){ .idNam <- names(P$data) }
  
  .dat <- list()
  .dat$fit <- data.frame("x" = as.factor(rep(.fitDat[[1]]$x, length(.fitDat))), 
                         "y" = as.vector( sapply(.fitDat, "[[", "y") ), 
                         "ty" = as.vector( sapply(.fitDat, "[[", "ty") ),  
                         "id" = rep(.idNam, each = length(.fitDat[[1]]$x)), 
                         "se" = as.vector( sapply(.fitDat, "[[", "se") ), 
                         stringsAsFactors = TRUE)
  if( asFact ){ .dat$fit$id <- as.factor( .dat$fit$id ) }
  
  # Drop base level if it is present
  if( length(basel) ) { .dat$fit <- .dat$fit[.dat$fit$x != basel, ] }
  
  .dat$misc <- list("trans" = trans)
  
  .pl <- ggplot(data = .dat$fit, aes("x" = id, "y" = ty)) + labs(title = NULL, y = P$ylab) +
         theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
  
  if (isMQGAM){ # Setting legend for MQGAMs
    if( asFact ){
      if( is.numeric(.idNam) ){ .idNam <- round(as.numeric(levels(.dat$fit$id)), 3) }
      .pl <- .pl + scale_x_discrete(labels = .idNam) + scale_colour_discrete(labels = .idNam)
    } else {
      if (min(diff(sort(.idNam)))>0.099) { # Ticks will be plotted if they are more than 10% apart, rounding error prevents >=0.1
        .pl <- .pl + scale_color_gradient(breaks = sort(.idNam, decreasing = T))
      }
    }
  }
  
  if( is.null(a.facet$facets) ){ a.facet$facets <- as.formula("~ x") }
  .pl <- .pl + do.call("facet_wrap", a.facet)
  
  return( structure(list("ggObj" = .pl, "data" = .dat, "type" = c("Multi", "Pterm", "Factor")), 
                    class = c("plotSmooth",  "gg")) )
  
}

