#' Checking conditional densities
#' 
#' @description XXX
#' @name cdCheck
#' @examples 
#' library(mgcViz);
#' 
#' Dataset where variance increases linearly with x2, for x2 > 0.2
#' n <- 1e3
#' x1 <- rnorm(1e3)
#' x2 <- rnorm(1e3)
#' dat <- data.frame("x1"=x1, "x2"=x2, "y"=sin(x1) + 0.5*x2^2 + pmax(x2, 0.2)*rnorm(n))
#' b <- gam(y ~ s(x1)+s(x2), data=dat)
#' 
#' cdCheck(b, x="x2")
#'
#' @importFrom KernSmooth dpik bkde bkde2D
#' @rdname cdCheck
#' @export cdCheck
cdCheck <- function(o, x, y=NULL, typeRes="deviance", n=c(80, 80), bw=NULL, 
                    xlim=NULL, ylim=NULL, palette=viridis(50, begin=0.2), rug=TRUE,
                    points=TRUE, cont=FALSE, maxp=1e4, tol=1e-6, aFun=NULL, dFun=NULL)
{
  
  ### 1. Preparation
  if( is.null(aFun) ){
    aFun <- function(.dx){
      return( .dx^0.2 )
    }
  }
  
  if( is.null(dFun) ){
    dFun <- function(.ed, .gr, .y){
      d <- dnorm(.gr, 0, sd=sd(.y))
      d <- sqrt(.ed) - sqrt(d)
      return( sign(d) * abs(d) ^ (1/3) )
    }
  }
  
  if( is.null(y) ){ y <- residuals(o, type = typeRes) }
  
  data <- o$model
  
  if( is.character(x) ){ # Get data from dataframe
    if( !(x %in% names(data)) ) stop("(x %in% names(data)) == FALSE")
    x <- data[[x]]
  }
  
  if(length(x) != length(y)){ stop("length(x) != length(y)") }
  
  # Discard data outside boundaries
  if( is.null(xlim) ){ 
    xlim <- range(x) 
  } else {
    tmp <- findInterval(x, xlim) == 1
    x <- x[ tmp ]  
    y <- y[ tmp ]
  }
  
  if( is.null(ylim) ){ 
    ylim <- range(y) 
  } else { 
    tmp <- findInterval(y, ylim) == 1
    x <- x[ tmp ]  
    y <- y[ tmp ] 
  }
  
  # Discard NAs
  good <- complete.cases(y, x)
  y <- y[ good ]
  x <- x[ good ]
  
  m <- length(good)
  
  ### 2. Density Estimation 
  if( is.null(bw) ){ bw <- c(dpik(x, range.x=xlim, gridsize=n[1]), 
                             dpik(y,  range.x=ylim, gridsize=n[2])) }
  
  estXY <- bkde2D(cbind(x, y), 
                  range.x = list(xlim, ylim), 
                  gridsize = n,
                  bandwidth = bw)
  
  estX <- bkde(x, gridsize = n[1], range.x = xlim, bandwidth = bw[1])
  
  estYcX <- estXY$fhat / estX$y
  
  estYcX[ estYcX <= tol*dnorm(0, 0, sd(y)) ] <- NA 
  
  ### 3. Plotting
  dat <- data.frame("z" = dFun(.ed=as.numeric(t(estYcX)), .gr=estXY$x2, .y=y), 
                    "x" = rep(estXY$x1, each=n[1]), 
                    "y" = rep(estXY$x2, n[2]), 
                    "dx" = rep(aFun(estX$y), each=n[1]))
  
  .pl <- ggplot(data = dat, aes(x=x, y=y, z=z)) + 
    geom_raster(aes_string(fill = "z", alpha = "dx")) + 
    scale_fill_gradientn(colours = palette, na.value="white") + 
    xlim(xlim[1], xlim[2]) + ylim(ylim[1], ylim[2])
  
  if( cont ){
    .pl <- .pl + geom_contour(color=1, na.rm=T) + labs(alpha="p(x)", fill="z")
  }
  
  if( rug || points){
    # Subsample some data
    if( m > maxp ){ tmp <- sample(1:m, maxp) } else { tmp <- 1:m }
    subS <- data.frame("x"=x[tmp], "y"=y[tmp])
    
    if( rug ){
      .pl <- .pl + geom_rug(data = subS, inherit.aes = F, aes(x = x, y = y), alpha = 0.2) 
    }
    
    if( points ){
      .pl <- .pl + geom_point(data = subS, inherit.aes = F, aes(x = x, y = y), shape = '.')
    }
  }
  
  .pl <-  .pl + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  .pl
}