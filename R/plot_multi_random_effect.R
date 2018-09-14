
#' @rdname plot.random.effect
#' @export plot.multi.random.effect
#' @export
#'
plot.multi.random.effect <- function(x, trans = identity, ...) {
  
  # 1) Prepare data
  tmp <- lapply(x, function(.inp) plot(.inp, trans = trans, ...))
  
  P <- list("data" = lapply(tmp, "[[", "data"), 
            "main" = gsub("\\d|,| |\\.", replacement = "", x = tmp[[1]]$ggObj$labels$title), 
            "xlab" = tmp[[1]]$ggObj$labels$x,
            "ylab" = tmp[[1]]$ggObj$labels$y)
  names( P$data ) <- names( x )
  
  # 2) Produce output object
  out <- .plot.multi.random.effect(P = P, trans = trans)
  
  class(out) <- c("plotSmooth", "gg")
  
  return(out)
}

############### Internal function
#' @noRd
.plot.multi.random.effect <- function(P = P, trans = trans) {
  
  .fitDat <- lapply(P$data, "[[", "fit")
  
  suppressWarnings( .idNam <- as.numeric(names(P$data)) )
  if( anyNA(.idNam) ){ .idNam <- names(P$data) }
  
  .dat <- list()
  .dat$fit <- data.frame("x" = as.vector( sapply(.fitDat, "[[", "x") ), 
                         "y" = as.vector( sapply(.fitDat, "[[", "y") ), 
                         "ty" = as.vector( sapply(.fitDat, "[[", "ty") ),  
                         "id" = as.factor(rep(.idNam, each = length(.fitDat[[1]]$x))))
  
  .dat$misc <- list("trans" = trans)
  
  if( is.numeric(.idNam) ){ .idNam <- round(.idNam, 3) }
  
  .pl <- ggplot(data = .dat$fit, aes("x" = x, "y" = ty, "colour" = id)) +
    labs(title = P$main, x = P$xlab, y = P$ylab) + 
    scale_colour_discrete(labels = .idNam) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  return( list("ggObj" = .pl, "data" = .dat, "type" = c("Multi", "RandomEffect")) ) 
}