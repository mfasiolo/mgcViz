
###### 
# Given some data `dat` this function bins it in `ngr` bin along `vx` and `vy`
# and the computes glyFun() using the data in each bin.
# The output is a data.frame where the $clu column indicated to which grid cell
# each row belongs to
#
.getGlyData <- function(dat, vx, vy, glyFun, ngr){
  
  # Build a grid: the data in each cell will be used to compute a glyph
  grX <- seq(min(dat[[vx]], na.rm = T), max(dat[[vx]], na.rm = T), length.out = ngr[1])
  grY <- seq(min(dat[[vy]], na.rm = T), max(dat[[vy]], na.rm = T), length.out = ngr[2])
  
  # Allocate each data point to a bin along x and y
  bnx <- findInterval(dat[[vx]], grX, rightmost.closed = T)
  bny <- findInterval(dat[[vy]], grY, rightmost.closed = T)
  
  # Unique bins
  bx <- as.numeric( names(table(bnx)) )
  by <- as.numeric( names(table(bny)) )
  nb1 <- length( bx )
  nb2 <- length( by )
  
  # We binned along x and y, not we need to allocate each data point to a unique 2D cell.
  # `clu` indicates to which cell each point belongs. binX and binY indicate the position
  # of each cell.
  n <- nrow( dat )
  clu <- numeric( n )
  binX <- binY <- numeric()
  kk <- 1
  for(ir in bx){
    for(ic in by){
      good <- which( bnx == ic & bny == ir )
      if( length(good) ){
        clu[good] <- kk
        binX <- c(binX, mean(c(grX[ic], grX[ic+1])))
        binY <- c(binY, mean(c(grY[ir], grY[ir+1])))
      }
      kk <- kk + 1
    }
  }
  
  # Transform data in each bin using glyFun
  bins <- as.numeric(names(table(clu)))
  mods <- lapply(1:length(bins), 
                 function(ii){
                   .bn <- bins[ii]
                   .sub <- dat[clu == .bn, ]
                   .out <- tryCatch( glyFun(.d = .sub), error = function(e) e )
                   if( "error" %in% class(.out) ){
                     return( NULL  )
                   } else {
                     .out$bn <- .bn
                     .out$Xc <- binX[ii]
                     .out$Yc <- binY[ii]
                     .out$clu <- ii
                     return( .out )
                   }
                 })

  mods <- do.call("rbind", mods)
  
  if( is.null(mods) ) { stop("glyFun() gave an error on all grid cells!") }
  
  return(mods)
  
}


