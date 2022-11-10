
# Jacobian for multinomial parametrization
# Jacobian of probabilitities alpha wrt regression coeff beta.
# o is gamObject, dat is data.frame, jj is index of alpha of interest
.multinomJacobian <- function(eta, jj, ...){
  
  alpha <- cbind(1, exp(eta)) / rowSums(cbind(1, exp(eta)))
  K <- ncol(alpha)
  
  # D alpha / D eta
  DaDe <- sapply(1:(K - 1), function(.kk) {
    alpha[, jj] * (as.numeric(jj == .kk + 1) - alpha[, .kk + 1])
  })
  if(nrow(alpha) == 1) { DaDe <- matrix(DaDe, nrow = 1) }
  
  return(DaDe)
  
}