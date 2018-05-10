
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
  
  .dat <- list()
  .dat$fit <- data.frame("x" = rep(.fitDat[[1]]$x, length(.fitDat)), 
                         "y" = as.vector( sapply(.fitDat, "[[", "y") ), 
                         "ty" = as.vector( sapply(.fitDat, "[[", "ty") ),  
                         "qu" = as.factor(rep(as.numeric(names(P$data)), each = length(.fitDat[[1]]$x))))
  
  .dat$misc <- list("trans" = trans)
  
  .pl <- ggplot(data = .dat$fit, aes("x" = x, "y" = ty, "colour" = qu)) +
    labs(title = P$main, x = P$xlab, y = P$ylab) + 
    scale_colour_discrete(labels = round(as.numeric(levels(.dat$fit$qu)), 3)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  return( list("ggObj" = .pl, "data" = .dat, "type" = c("Multi", "RandomEffect")) ) 
}