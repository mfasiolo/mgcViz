
#' @rdname plot.mgcv.smooth.1D
#' @export plot.multi.mgcv.smooth.1D
#' @export
#'
plot.multi.mgcv.smooth.1D <- function(x, n = 100, xlim = NULL, maxpo = 1e4, trans = identity,
                                      unconditional = FALSE, seWithMean = FALSE, asFact = NULL, ...) {
  
  if( inherits(x, "multi.fs.interaction.1D") ){
    stop("Don't know how to plot effects of class \"multi.fs.interaction.1D\"")
  }
  
  # 1) Prepare data
  tmp <- lapply(x, function(.inp) plot(.inp, n = n, xlim = xlim, maxpo = maxpo, trans = trans, 
                                       unconditional = unconditional, seWithMean = seWithMean, ...))
  
  P <- list("data" = lapply(tmp, "[[", "data"), 
            "main" = tmp[[1]]$ggObj$labels$title, 
            "xlab" = tmp[[1]]$ggObj$labels$x,
            "ylab" = gsub("\\d|,| |\\.", replacement = "", x = tmp[[1]]$ggObj$labels$y))
  names( P$data ) <- names( x )
  
  
  
  # 2) Produce output object
  out <- .plot.multi.mgcv.smooth.1D(P = P, trans = trans, 
                                    asFact = asFact, isMQGAM = attr(x, "isMQGAM"))
  
  class(out) <- c("plotSmooth", "gg")
  
  return(out)
}

############### Internal function
#' @noRd
.plot.multi.mgcv.smooth.1D <- function(P, trans, asFact, isMQGAM) {
  
  if( !is.null(asFact) && asFact == "force" ){ 
    asFact <- TRUE
    isMQGAM <- FALSE 
  }
  
  .fitDat <- lapply(P$data, "[[", "fit")
  
   suppressWarnings( .idNam <- as.numeric(names(P$data)) )
   if( anyNA(.idNam) ){ .idNam <- names(P$data) }
  
  .dat <- list()
  .dat$fit <- data.frame("x" = rep(.fitDat[[1]]$x, length(.fitDat)), 
                         "y" = as.vector( sapply(.fitDat, "[[", "y") ), 
                         "ty" = as.vector( sapply(.fitDat, "[[", "ty") ),  
                         "id" = rep(.idNam, each = length(.fitDat[[1]]$x)), 
                         stringsAsFactors = TRUE)
  if( !isMQGAM ){ 
    .dat$fit$id <- if(is.null(asFact)||asFact){ as.factor(.dat$fit$id) } else { as.numeric(.dat$fit$id) }
  }
  
  .dat$res <- P$data[[1]]$res
  .dat$res$y <- NULL
  
  .dat$misc <- list("trans" = trans)
  
  .pl <- ggplot(data = .dat$fit, aes("x" = x, "y" = ty, "colour" = id, "group" = id)) +
    labs(title = P$main, x = P$xlab, y = P$ylab) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  if (isMQGAM){ # Setting legend for MQGAMs
    if (is.null(asFact)){ asFact <- length(.idNam) < 10 }
    if (asFact == TRUE) {
       .pl <- .pl + scale_color_gradient(breaks = round(sort(.idNam, decreasing = TRUE), 3)) +
        guides(color = guide_legend(override.aes = list(size = 5)))
    } else {
      if (min(diff(sort(.idNam)))>0.099) { # Ticks will be plotted if they are more than 10% apart, rounding error prevents >=0.1
        .pl <- .pl + scale_color_gradient(breaks = sort(.idNam, decreasing = TRUE))
      }
    }
  } 
  
  return( list("ggObj" = .pl, "data" = .dat, "type" = c("Multi", "1D")) ) 
  
}
