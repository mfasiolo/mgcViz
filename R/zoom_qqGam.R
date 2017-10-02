#' Allow to efficiently zoom on a qqGam object
#' 
#' @description XXX
#' @name zoom.qqGam
#' @examples 
#' library(mgcViz);
#' set.seed(0)
#' n.samp <- 500
#' dat <- gamSim(1,n=n.samp,dist="binary",scale=.33)
#' p <- binomial()$linkinv(dat$f) ## binomial p
#' n <- sample(c(1,3),n.samp,replace=TRUE) ## binomial n
#' dat$y <- rbinom(n,n,p)
#' dat$n <- n
#' lr.fit <- bam(y/n ~ s(x0) + s(x1) + s(x2) + s(x3)
#'               , family = binomial, data = dat,
#'               weights = n, method = "REML")
#' 
#' set.seed(414)
#' o <- qq.gam(lr.fit, rep = 50, method = "simul1", CI = "normal")
#' o # This is the whole qqplot
#' 
#' # We can zoom in along x at little extra costs (most computation already done by qq.gam)
#' zoom(o, xlim = c(0, 1), show.reps = T, 
#'      a.replin = list(alpha = 0.1), a.qqpoi =  list(shape = 19))
#' @rdname zoom.qqGam
#' @export zoom.qqGam
zoom.qqGam <- function(o, xlim = NULL, ylim = NULL, discrete = NULL, ngr = 1e3,
                       adGrid = TRUE, CI = FALSE, worm = FALSE, show.reps = FALSE, 
                       a.qqpoi = list(), a.ablin = list(), a.cipoly = list(), 
                       a.replin = list(), ...) {
  
  a.all <- .argMaster("qq.gam")
  for(nam in names(a.all)){
    a.all[[nam]] <- .argSetup(a.all[[nam]], get(nam), nam, verbose = FALSE)
  }
  
  P <- o$store
  # Subset data according to xlim
  if (!is.null(xlim) && adGrid) {
    good <- which(P$Dq > xlim[1] & P$Dq < xlim[2])
    P$Dq <- P$Dq[good]
    P$D  <- P$D[good]
    P$dm <- P$dm[good, ]
    P$conf <- P$conf[, good]
  }
  if(is.null(discrete)) discrete <- length(P$Dq) > 1e4 
  P <- .discretize.qq.gam(P = P, discrete = discrete, ngr = ngr,
                          CI = CI, show.reps = show.reps)
  .pl <- .plot.qq.gam(P = P, CI = CI, worm = worm, show.reps = show.reps,
                      xlimit = xlim, ylimit = ylim, a.all = a.all)
  return(.pl)
}


