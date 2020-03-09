
# Jacobian for multinomial parametrization
# Jacobian of probabilitities alpha wrt regression coeff beta.
# o is gamObject, dat is data.frame, jj is index of alpha of interest
.multinomJacobian <- function(o, dat, jj, ...){
  
  # NB stats:: needed otherwise we end up using the predict() defined
  # in the family!!
  eta <- predict(o, newdata = dat, ...)
  alpha <- cbind(1, exp(eta)) / rowSums(cbind(1, exp(eta)))
  K <- ncol(alpha)
  
  # D alpha / D eta
  DaDe <- sapply(1:(K - 1), function(.kk) {
    alpha[, jj] * (as.numeric(jj == .kk + 1) - alpha[, .kk + 1])
  })
  if(nrow(alpha) == 1) { DaDe <- matrix(DaDe, nrow = 1) }
  
  # Dalpha/Dbeta = Dalpha/Deta * Deta/Dbeta
  X <- model.matrix(o, newdata = dat)
  lpi <- attr(X, "lpi")
  if( is.null(lpi) ) { lpi <- list(1:ncol(X)) }
  
  J <- lapply(seq_along(lpi), function(ii) {
    X[ , lpi[[ii]], drop = FALSE] * DaDe[, ii]
  })
  J <- do.call("cbind", J)
  
  return( J )
}