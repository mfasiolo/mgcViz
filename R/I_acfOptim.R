
.acfOptim <- function(dat, fn){
  
  out <- list()
  
  ### 1) Find direction on X-Y which maximizes absolute value of acf  
  if( is.null(fn[[1]]) || is.null(fn[[2]]) ){
    
    # Subsample data if dataset too large
    tmpS <- if( nrow(dat)<1e5 ){ dat } else { sample_n(dat, 1e5) }
    
    # Search best direction or angle on a grid
    thSeq <- seq(0, pi/2, length.out = 20)
    fval <- sapply(thSeq, 
                   function(.in){
                     .tmp <-tmpS$z[ order(cos(.in)*tmpS$x + sin(.in)*tmpS$y) ]
                     return( sum(abs(acf(.tmp, plot=F)$acf[-1])) )
                   })
    theta <- thSeq[ which.max(fval) ] # Pick best angle
    
    # Create functions that rotate x and y
    tmpCreator <- function(.th) function(.x, .y) cos(.th)*.x+sin(.th)*.y
    if( is.null(fn[[1]]) ){
      fn[[1]] <- tmpCreator(theta)
      if( is.null(fn[[2]]) ){ fn[[2]] <- tmpCreator(theta+pi/2) }
    } else {
      if( is.null(fn[[2]]) ){ fn[[2]] <- tmpCreator(theta) }
    }
  }
  
  ### 2) Produce data for plotting 
  df <- list()
  ci <- numeric(2)
  for(ii in 1:2){ 
    bacf <- acf(dat$z[order(fn[[ii]](dat$x, dat$y))], plot = FALSE)
    ci[ii] <- qnorm((1 + 0.95)/2)/sqrt(bacf$n.used)
    bacfdf <- with(bacf, data.frame(lag, acf))
    df[[ii]] <- bacfdf[-1, ]
  }

  return( list("fn" = fn, "df" = df, "ci" = ci) )
}