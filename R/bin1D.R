bin1D <- function(x, y, ngr, dfun = NULL, tol = 1e-6)
{
  if( is.null(dfun) ){ dfun <- function(.x, .y) c(0, cumsum( sqrt(diff(.x)^2 + diff(.y)^2) )) }
  v <- dfun(x, y)
  
  n <- length(v)
  gr <- seq(v[1], v[n], length.out = ngr)
  xm <- ym <- count <- list()
  
  cb <- 1
  xcb <- 0
  ycb <- 0
  cobc <- 0
  for(ii in 1:(n+1)){
    binned <- FALSE
    while( !binned ){
      if( ii <= n && v[ii] > gr[cb]-tol && v[ii] < gr[cb+1]+tol ){
        xcb <- xcb + x[ii]
        ycb <- ycb + y[ii]
        cobc <- cobc + 1
        binned <- TRUE
      } else {
        if(cobc > 0){
          xm <- append(xm, xcb/cobc)
          ym <- append(ym, ycb/cobc)
          count <- append(count, cobc)
          xcb <- 0
          ycb <- 0
          cobc <- 0
        }
        cb <- cb + 1
        if(ii == n+1) { break }
      }
    }
  }
  return( list("xm"=unlist(xm), "ym"=unlist(ym), "count"=unlist(count)) )
}