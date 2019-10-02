
#' @rdname plot.ptermNumeric
#' @export plot.ptermMatrixNumeric
#' @export
#' 
plot.ptermMatrixNumeric <- function(x, n = 100, xlim = NULL, trans = identity, ...){
  
  if(x$order > 1){ 
    message("mgcViz does not know how to plot this effect. Returning NULL.")
    return( invisible(NULL) ) 
  }
  
  gObj <- x$gObj
  
  # 1) Do prediction
  X <- gObj$model
  if(n > nrow(X)){ # Model matrix too short, we make it longer
    X <- X[rep(1:nrow(X), ceiling(n/nrow(X))), ]
  }
  data <- X[1:n, ]
  
  # We get the original data set, which contains also variables used to build the MatrixNumeric effects
  data0 <- eval(gObj$call$data, environment(formula(gObj)) )
  
  if( is.null(xlim) ){ xlim <- range(data0[[x$varNam]]) }
  xx <- seq(xlim[1], xlim[2], length = n)
  
  # Here x$name contains the call used to build this effect (e.g. bs(x0, degree = 2)), here we 
  # transform it to bs(xx, degree = 2) and the we call this to create the relevant part of the model
  # matrix
  exprs <- gsub(x$varNam, "xx", x$name)
  data[[x$name]] <- eval( str2expression(exprs) )
  data[[x$varNam]] <- xx
  
  # Suppressing spurious warnings from predict.gam
  .pred <- withCallingHandlers(predict.gam(gObj, type = "terms", se.fit = TRUE, terms = x$nam, newdata = data), 
                               warning = function(w){ 
                                 if(is.list(gObj$formula) && any(grepl("is absent, its contrast will be ignored", w))){ 
                                   invokeRestart( "muffleWarning" )
                                 }
                               })
  
  # 2) Build dataset on fitted effect
  .dat <- list()
  .dat$fit <- data.frame("x"  = xx,
                         "y"  = unname(.pred$fit),
                         "ty"  = trans( unname(.pred$fit) ),
                         "se" = unname(.pred$se) )
  
  # 3) Build output plot
  .dat$misc <- list("trans" = trans)
  
  .pl <- ggplot(data = .dat$fit, aes("x" = x, "y" = ty)) +
    labs(title = NULL, x = x$varNam, y = x$name) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  return( structure(list("ggObj" = .pl, "data" = .dat, "type" = c("Pterm", "Matrix", "Numeric")), 
                    class = c("plotSmooth", "gg")) )
  
}
