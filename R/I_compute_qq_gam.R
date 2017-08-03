##############
# Function that computes all the stuff necessary to plot the qqplots 
# OUTPUT: a list where
# - D is a vector of sorted residuals (i.e. the points in the qqplot)
# - Dq is a vector of theoretical quantiles
# - Dm is a matrix (n X rep) where each column contains n simulated quantiles
# - conf is a matrix (2 X n) where the first (second) row contains 
#        the lower (upper) confidence interval
# - method indicates which method has been used to calculate the qqplot
# - ylab is a string indicating what we are plotting on the y axis
##############
.compute.qq.gam <- function(o, type, method, CI, level = level, rep, sortFun) {
  ## It's come out of a gamm fitter and qq.gam can't see the random effects
  ## that would be necessary to get quantiles. Fall back to normal QQ plot.
  if (o$method %in% c("PQL", "lme.ML", "lme.REML", "lmer.REML", "lmer.ML", "glmer.ML")) {
    method <- "normal"
  }
  out <- NULL
  if (method %in% c("simul1", "simul2")) {
    out <- .compute.qq.gam.simul(o = o, type = type, method = method, CI = CI, 
                                 level = level, rep = rep, sortFun = sortFun)
  }
  if (method %in% c("tnormal", "tunif")) {
    out <- .compute.qq.gam.tunif.tnormal(o = o, method = method, CI = CI, 
                                         level = level, sortFun = sortFun)
  }
  if (is.null(out$Dq)) method <- "normal"
  if (method == "normal") {
    out <- .compute.qq.gam.normal(o = o, type = type, method = method, CI = CI, 
                                  level = level, sortFun = sortFun)
  }
  return(out)  
}

##############
# Simulation based approaches from "On quantile quantile plots for generalized linear models"
# of Augustin and Wood 2012
##############
.compute.qq.gam.simul <- function(o, type, method, CI, level, rep, sortFun) {
  D <- sortFun(residuals(o, type = type))
  Dq <- dm <- conf <- NULL
  n <- length(D)
  if (method == "simul2"){
    CI <- "none" # CI do not make sense with this method
    fam <- fix.family.qf(o$family)
    ix <- frankv(D) # Or should this be 1:n??
    U <- (ix - 0.5)/n
    if (!is.null(fam$qf)) { # If CDF not available use first method
      dm <- list()
      indx <- 1:n
      for (ii in 1:rep) {
        # Randomly shuffle the uniform quantiles U
        jj <- sample(indx, n)
        indx[jj] <- indx
        o$y <- fam$qf(U[indx], o$fitted.values, o$prior.weights, o$sig2)
        dm[[ii]] <- sortFun(residuals(o, type = type)[jj])
        indx <- indx[jj]
      }
      dm <- do.call("cbind", dm)
      Dq <- rowMeans(dm)
    } else {
      method <- "simul1"
    }
  }
  if (method == "simul1") {
    fam <- fix.family.rd(o$family)
    if (!is.null(fam$rd)) {
      dm <- list()
      for (ii in 1:rep) {
        yr <- fam$rd(o$fitted.values, o$prior.weights, o$sig2)
        o$y <- yr
        dm[[ii]] <- sortFun(residuals(o, type = type))
      }
      dm <- do.call("cbind", dm)
      Dq <- .quBySort(as.numeric(dm), (1:n - 0.5)/n, sortFun)
    }
  }
  
  # Calculate confidence intervals
  if( !is.null(Dq) )
  {
    alpha <- (1 - level)/2
    if (CI == "quantile") {
      conf <- rbind(rowOrderStats(dm, which = ceiling(rep * alpha)),
                    rowOrderStats(dm, which = ceiling(rep * (1 - alpha))))
    }
    if (CI == "normal") {
      sdq <- rowSds(dm)
      conf <- rbind(qnorm(alpha, Dq, sdq), qnorm(1 - alpha, Dq, sdq))
    }
  }
  
  return(list(
    "D" = D, "Dq" = Dq, "dm" = dm, "conf" = conf, 
    "method" = method, "ylab" = paste(type, "residuals")
  ))
}


##############
# Transform data y (not residuals!!) to uniform or normal and then compute
# qqplots on those
##############
.compute.qq.gam.tunif.tnormal <- function(o, method, CI, level, rep, sortFun) {
  D <- o$y
  Dq <- dm <-  conf <- NULL
  n <- length(D)
  fam <- fix.family.cdf(o$family)
  if (!is.null(fam$cdf)) {
    Dq <- p <- (1:n - 0.5)/n
    D <- sortFun(fam$cdf(D, o$fitted.values, o$prior.weights, o$sig2, logp = TRUE))
    if (method == "tnormal") {
      Dq <- qnorm(Dq)
      D <- qnorm(D, log.p = TRUE )
    } else {
      D <- exp(D)
    }
    if (CI != "none") {
      alpha <- (1 - level)/2
      if (method == "tnormal") { # Normal CI (from "Worm plot: a simple diagnostic device
        # for modelling growth reference curves") or ...
        tmp <- qnorm(alpha) * sqrt(p * (1 - p)/n) / dnorm(Dq)
        conf <- rbind(Dq + tmp, Dq - tmp)
      } else { # ... uniform CI (from distribution of uniform order stats (see Wikip))
        tmp <- 1:n
        conf <- rbind(qbeta(alpha, tmp, n - tmp + 1),
                      qbeta(1 - alpha, tmp, n - tmp + 1))
      }
    }
  }
  ylab <- ifelse(method == "tnormal",
                 "Obs y transf to normal", "Obs y transf to unif")
  return(list(
    "D" = D, "Dq" = Dq, "dm" = dm, "conf" = conf,
    "method" = method, "ylab" = ylab
  ))
}


##############
# Simplest possible normal qqplot on residuals
##############
.compute.qq.gam.normal <- function(o, type, method, CI, level, sortFun) {
  D <- sortFun(residuals(o, type = type))
  D <- D / sd(D)
  n <- length(D)
  p <- (1:n - 0.5)/n
  Dq <- qnorm(p)
  dm <- conf <- NULL
  if (CI != "none") {
    alpha <- (1 - level)/2
    tmp <- qnorm(alpha) * sqrt(p * (1 - p)/n) / dnorm(Dq)
    conf <- rbind(Dq + tmp, Dq - tmp)
  }
  return(list(
    "D" = D, "Dq" = Dq, "dm" = dm, "conf" = conf, 
    "method" = method, "ylab" = paste("normalize", type, "residuals")
  ))
}
