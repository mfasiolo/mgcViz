##############
# Prepare ALE effects
# This function will be called by ALE.gam
#############
#' @noRd
#' @export
.prepare.ALE <- function(o, xnam, data, type, K, bins, predFun, jacFun, varFun, center, ...) {
  
  .x <- data[[xnam]]
  
  if( inherits(.x, "factor") || inherits(.x, "character") ) {
    .x <- as.factor(.x)
    .cls <- "factor"
    .data <- .prepare.ALE.factor(o = o, xnam = xnam, data = data, type = type, K = K, bins = bins,
                                 predFun = predFun, jacFun = jacFun, varFun, center = center, ...)
  }
  if( inherits(.x, "numeric") || inherits(.x, "integer") ) {
    .cls <- "numeric"
    .data <- .prepare.ALE.numeric(o = o, xnam = xnam, data = data, type = type, K = K, bins = bins,
                                  predFun = predFun, jacFun = jacFun, varFun, center = center, ...)
  }
  
  .out <- structure(list("ALE" = .data, "type" = .cls, "xval" = .x, "xnam" = xnam),
                    class = "ALE1D")
  return( .out )
  
}


##############
# Prepare ALE effects: factor case
#############
.prepare.ALE.factor <- function(o, xnam, data, type, K, bins, predFun, jacFun, varFun, center, ...) {
  ####
  ## The code here is based on the ALEPLot package of Dan Apley. 
  ## See https://cran.r-project.org/web/packages/ALEPlot/index.html
  ####
  
  n = dim(data)[1]
  d = dim(data)[2]
  
  x <- data[[xnam]]
  J <- which(names(data) == xnam)
  
  x <- droplevels(x)
  xcount <- as.numeric(table(x))
  xprob <- xcount/sum(xcount)
  nlv <- nlevels(x)
  Dcum <- matrix(0, nlv, nlv)
  D <- matrix(0, nlv, nlv)
  for (jj in setdiff(1:d, J)) {
    if ( inherits(data[, jj], "factor") ) {
      A = table(x, data[ , jj])
      A = A / xcount
      for (ii in 1:(nlv - 1)) {
        for (kk in (ii + 1):nlv) {
          D[ii, kk] = sum(abs(A[ii, ] - A[kk, ]))/2
          D[kk, ii] = D[ii, kk]
        }
      }
      Dcum <- Dcum + D
    } else {
      qxall <- quantile(data[ , jj], probs = seq(0, 1, length.out = 100), na.rm = TRUE, names = FALSE)
      xecdf = tapply(data[ , jj], x, ecdf)
      for (ii in 1:(nlv - 1)) {
        for (kk in (ii + 1):nlv) {
          D[ii, kk] = max(abs(xecdf[[ii]](qxall) - xecdf[[kk]](qxall)))
          D[kk, ii] = D[ii, kk]
        }
      }
      Dcum <- Dcum + D
    }
  }
  # Distances of each level from all the rest (e.g. c(0.5,-0.5,0))
  D1D <- cmdscale(Dcum, k = 1)
  # Order of the level (e.g. c(2,3,1))
  indOrd <- sort(D1D, index.return = TRUE)$ix
  # Actual order of levels (e.g. c(3,1,2))
  ordInd <- sort(indOrd, index.return = TRUE)$ix
  # Order factor levels according to distance
  levsOrig <- levels(x)
  levsOrd <- levsOrig[indOrd]
  # Integer vect indicating right order of observed levels (e.g. x=c(1,2,3) becomes (3,1,2))
  xOrd <- ordInd[as.numeric(x)]
  # Indexes of obs excluding the last (rowInd2) and the first (rowInd1)
  rowInd2 <- (1:n)[xOrd < nlv]
  rowInd1 <- (1:n)[xOrd > 1]
  xOrd2 <- xOrd[rowInd2]
  xOrd1 <- xOrd[rowInd1]
  X2 <- data[rowInd2, ]
  X1 <- data[rowInd1, ]
  # In X2 we increase class by 1, so levels z_2, z_3, etc become associated with
  # extra variables x_1, x_2. While in X1 we decrease, so levels z_1, z_2, etc
  # go with x_2, x_3 in the predictions.
  X2[[xnam]] <- levsOrd[xOrd2 + 1]
  X1[[xnam]] <- levsOrd[xOrd1 - 1]
  mu <- as.matrix( predFun(.o = o, .d = data, .t = type, se = FALSE, ...) )
  mu2 <- as.matrix( predFun(.o = o, .d = X2, .t = type, se = FALSE, ...) )
  mu1 <- as.matrix( predFun(.o = o, .d = X1, .t = type, se = FALSE, ...) )
  # delta2 = f(z_k, x_{k-1}) - f(z_{k-1}, x_{k-1}) for k = 1, 2, 3, ..., K
  # delta1 = f(z_k, x_k) - f(z_{k-1}, x_k) for k = 1, 2, 3, 4, ..., K
  delta2 <- mu2 - mu[rowInd2]
  delta1 <- mu[rowInd1] - mu1
  # ntot_k = n(k) + n(k-1) for k = 1, ..., K (where K = nlv - 1 = number of intervals)
  ntot <- as.vector( table(xOrd2) + table(xOrd1) )
  # sum over factor levels and the divide by total count in each interval
  delta2 <- as.numeric(tapply(delta2, xOrd2, sum))
  delta1 <- as.numeric(tapply(delta1, xOrd1, sum))
  delta <- (delta2 + delta1) / ntot
  fx <- c(0, cumsum(delta))
  x <- factor(levsOrd, levels = levsOrd)
  
  # Now we get cov(fx) using the delta method, we need derivatives of ALE effect f(z) wrt beta (model parameters)
  # Given that above we calculate delta1 and delta2 separately, here we get Jacobian corresponding to
  # delta2 and delta1 separately. Then we aggregate them by scaling using ntot.
  # Dfx is (K+1) x p matrix with p = length(beta) and K = number of intervals 
  data1 <- data[rowInd1, , drop = FALSE]
  data2 <- data[rowInd2, , drop = FALSE]
  Dfx2 <- lapply(1:(nlv-1), function(.ii) colSums(jacFun(.o = o, .d = X2[X2[[xnam]] == x[.ii+1], ], .t = type, ...)$J - 
                                                  jacFun(.o = o, .d = data2[data2[[xnam]] == x[.ii], ], .t = type, ...)$J))
  Dfx1 <- lapply(2:nlv, function(.ii) colSums(jacFun(.o = o, .d = data1[data1[[xnam]] == x[.ii], ], .t = type, ...)$J - 
                                              jacFun(.o = o, .d = X1[X1[[xnam]] == x[.ii-1], ], .t = type, ...)$J))
  Dfx <- ( do.call("rbind", Dfx2) + do.call("rbind", Dfx1) ) / ntot
  Dfx <- apply(Dfx, 2, cumsum)
  Dfx <- rbind(0, Dfx) # First row is 0 because f(z_0) is 0
  
  # Now we center the ALE effect...
  if( center == 1 ) { # ... by substracting E(fx), as in Apley's paper
    fx <- fx - sum(fx * xprob[indOrd])
    Dfx <- t(t(Dfx) - drop(t(Dfx) %*% xprob[indOrd]))
  }
  if( center == 2 ){ # ... by adding E(mu(z_0))
    fx <- fx + mean(mu[data[[xnam]] == x[1]])
    Dfx <- t(t(Dfx) + colMeans(jacFun(.o = o, .d = data[data[[xnam]] == x[1], ], .t = type, ...)$J))
  }
  
  # Compute Jacobian once (the actual data it unimportant), to check whether it's sparse
  V <- Dfx %*% varFun(.o = o, .t = type, .J = jacFun(.o = o, .d = X2[1:2, ], .t = type, ...), ...) %*% t(Dfx)
  
  out <- list("ALE" = data.frame(x = x, y = fx, se = sqrt(diag(V))), "varALE" = V)
  
  return( out )
  
}


##############
# Prepare ALE effects: numeric case
#############
.prepare.ALE.numeric <- function(o, xnam, data, type, K, bins, predFun, jacFun, varFun, center, ...) {
  ####
  ## The code here is based on the ALEPLot package of Dan Apley. 
  ## See https://cran.r-project.org/web/packages/ALEPlot/index.html
  ####
  
  n <- nrow(data)
  d <- ncol(data)
  
  x <- data[[xnam]]
  J <- which(names(data) == xnam)
  
  # Create bins along x_j
  z <- bins
  if( is.null(z) ){
    z <- as.numeric(quantile(x,seq(1/K,1,length.out=K), type=1))
  }
  z <- unique(c(min(x), z)) # necessary when z could have repeated values 
  K <- length(z) - 1 # set K to the number of intervals on z
  
  # Bin the data: i-th entry of bnum indicates the bin to which x_ij belongs
  bnum <- as.numeric(cut(x, breaks=z, include.lowest=TRUE)) 
  
  # Predicts at z_0,...,z_K and z_1, ..., z_{K+1}
  X1 <- data
  X2 <- data
  X1[[xnam]] <- z[bnum]
  X2[[xnam]] <- z[bnum+1]
  mu1 <- predFun(.o = o, .d = X1, .t = type, se = FALSE, ...)
  mu2 <- predFun(.o = o, .d = X2, .t = type, se = FALSE, ...)
  
  # Get local effects f(z_k|x_i) - f(z_{k-1}|x_i) and average them by bin 
  delta <- mu2 - mu1  
  delta <- as.numeric( tapply(delta, bnum, mean) )
  
  # ALE effects at z_0 = 0, z_1, ..., z_K
  fx = c(0, cumsum(delta))
  
  # number of x_ij's falling in each bin
  b1 <- as.numeric( table(bnum) ) 
  
  # Now we get cov(fx) using the delta method
  # Get derivatives of ALE effect f(z) wrt beta, for z_1, ..., z_K
  # Dfx is (K+1 x p) matrix with p = length(beta) 
  Dfx <- lapply(1:K, function(.bn) colMeans(jacFun(.o = o, .d = X2[bnum == .bn, ], .t = type, ...)$J -  
                                            jacFun(.o = o, .d = X1[bnum == .bn, ], .t = type, ...)$J))
  Dfx <- do.call("rbind", Dfx)
  Dfx <- apply(Dfx, 2, cumsum) 
  Dfx <- rbind(0, Dfx) # First row is 0 because f(z_0) is 0

  # Now we center the ALE effect...
  if( center == 1 ) { # ... by substracting E(fx), as in Apley's paper
    fx <- fx - sum((fx[1:K]+fx[2:(K+1)])/2*b1)/sum(b1)
    Dfx <- t(t(Dfx) - colSums((Dfx[1:K, ] + Dfx[2:(K+1), ])/2 * b1) / sum(b1))
  }
  if( center == 2 ){ # ... by adding E(mu(z_0))
    fx <- fx + mean(mu1[bnum == 1])
    Dfx <- t(t(Dfx) + colMeans(jacFun(.o = o, .d = X1[bnum == 1, ], .t = type, ...)$J))
  }

  # Compute Jacobian once (the actual data it unimportant), to check whether it's sparse
  V <- Dfx %*% varFun(.o = o, .t = type, .J = jacFun(.o = o, .d = X2[1:2, ], .t = type, ...), ...) %*% t(Dfx)
  
  out <- list("ALE" = data.frame(x = z, y = fx, se = sqrt(diag(V))), "varALE" = V)
  
  return( out )
  
}
