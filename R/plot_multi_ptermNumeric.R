
#' @rdname plot.ptermNumeric
#' @export plot.multi.ptermNumeric
#' @export
#'
plot.multi.ptermNumeric <- function(x, ...) {
  
  if(x[[1]]$order > 1){ 
    message("mgcViz does not know how to plot this effect. Returning NULL.")
    return( invisible(NULL) ) 
  }
  
  # 1) Prepare data
  nm <- x[[1]]$name
  qus <- as.numeric( names(x) )
  
  tmp <- lapply(x, function(.x){ 
    .o <- summary(.x$gObj)$p.table
    return( .o[which(rownames(.o) == nm), ] )
  })
  
  P <- list()
  P$data$fit <- data.frame("x" = as.factor(qus), 
                           "y" = sapply(tmp, "[", 1), 
                           "ty" = sapply(tmp, "[", 1), 
                           "se" = sapply(tmp, "[", 2))
  P$ylab <- paste0("coeff ", nm)
  P$xlab <- "qu"
  P$data$misc <- list("trans" = identity)

  # 2) Produce output object
  out <- .plot.multi.ptermNumeric(P = P)
  
  class(out) <- c("plotSmooth", "gg")
  
  return(out)
}

############### Internal function
#' @noRd
.plot.multi.ptermNumeric <- function(P = P) {
  
  .pl <- ggplot(data = P$data$fit, aes("x" = x, "y" = ty)) +
    labs(title = NULL, x = P$xlab, y = P$ylab) + 
    scale_x_discrete(labels = round(as.numeric(levels(P$data$fit$x)), 3)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
  
  return( structure(list("ggObj" = .pl, "data" = P$data, "type" = c("Multi", "Pterm", "Numeric")), 
                    class = c("plotSmooth",  "gg")) )
  
}
