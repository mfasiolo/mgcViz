
#' .initializeXXX
#'
#' @param o, ... 
#' @param unconditional, ... 
#' @param residuals, ... 
#' @param resDen, ... 
#' @param se, ... 
#' @param fv.terms, ...
#' @return a list
#' @noRd
.initialize <- function(o, unconditional, residuals, resDen, se, se.mult) {
  
  V <- fv.terms <- NULL 
  
  # Use Bayesian cov matrix including smoothing parameter uncertainty?
  if (unconditional) { 
    if ( is.null(o$gObj$Vc) ) { 
      warning("Smoothness uncertainty corrected covariance not available") 
    } else { 
      V <- o$gObj$Vc 
    } 
  }
  
  w.resid <- NULL
  if (length(residuals) > 1) { # residuals supplied 
    if (length(residuals) == length(o$gObj$residuals)) { 
      w.resid <- residuals 
    } else { 
      warning("residuals argument to plot.gamViz is wrong length: ignored") 
    }
    partial.resids <- TRUE
  } else {
    partial.resids <- residuals
  } # use working residuals or none
  
  # Getting information needed for partial residuals
  if (partial.resids || (resDen != "none")) { 
    if (is.null(w.resid)) { # produce working residuals if info available
      if (is.null(o$gObj$residuals) || is.null(o$gObj$weights)) {
        partial.resids <- FALSE
      } else {
        wr <- sqrt(abs(o$gObj$weights))
        w.resid <- o$gObj$residuals * wr
      }
    }
    
    fv.terms <- o$gObj$store$termsFit[ , o$gObj$store$np + o$ism]
    if(is.null(fv.terms)) { 
      fv.terms <- predict(o$gObj, type = "terms")
    }
  }
  
  if (se) { 
    # Sort out CI widths for 1D and 2D smooths
    if (se.mult < 0) { se.mult <- 0 }
    # Check that variances are actually available
    if ( o$gObj$Vp[1,1] < 0 ){  
      se <- FALSE
      warning("No variance estimates available")
    }
  } 
  
  ## Array giving the order of each parametric term
  order <- if (is.list(o$gObj$pterms)) {
    unlist(lapply(o$gObj$pterms, attr, "order")) 
  } else {
    attr(o$gObj$pterms, "order") 
  }
  
  return(list(
    V = V,
    order = order,
    w.resid = w.resid,
    se.mult = se.mult,
    se = se,
    fv.terms = fv.terms,
    partial.resids = partial.resids
  ))
}