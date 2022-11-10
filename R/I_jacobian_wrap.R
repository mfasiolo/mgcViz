
# Wrap jacobian function provided by family
# o is gamObject, dat is data.frame, jj is index of response of interest
.jacobian_wrap <- function(o, dat, jj, ...){
  
  eta <- as.matrix(predict(o, newdata = dat, ...))
  
  X <- model.matrix(o, newdata = dat)
  lpi <- attr(o$formula, "lpi")
  if( is.null(lpi) ) { lpi <- list(1:ncol(X)) }
  
  # Contains the matrix DmuDeta or a list with further info
  Dmu <- o$family$jacobian(eta = eta, jj = jj)
  
  theta_idx <- NULL
  # Sparse case? Jacobian of j-th output depends only on some linear predictors (and/or extra parameters) OR...
  if( is.list(Dmu) ){
    eta_idx <- Dmu$eta_idx
    theta_idx <- Dmu$theta_idx
    DmuDeta <- Dmu$DmuDeta
    lpi <- lpi[eta_idx]
  } else { # ... dense Jacobian
    DmuDeta <- Dmu
  }
  
  # Jacobian with derivative w.r.t. i-th linear predictor (eta) on i-th columns
  # Dmu/Dbeta = Dmu/Deta * Deta/Dbeta
  J <- lapply(seq_along(lpi), function(ii) {
    X[ , lpi[[ii]], drop = FALSE] * DmuDeta[, ii]
  })

  # Add Jacobian columns for derivatives w.r.t. extra parameters (theta)
  if(length(theta_idx)){
    for(ii in seq_along(theta_idx)){
      J[[length(J)+1]] <- Dmu$DmuDtheta[, ii]
    }
  }
  
  J <- do.call("cbind", J)
  
  return( list("J" = J, "lpi" = lpi, "theta_idx" = theta_idx) )
}

