
#' .initializeXXX
#'
#' @param o, ... 
#' @param unconditional, ... 
#' @param residuals, ... 
#' @param resDen, ... 
#' @param se, ... 
#' @param fv.terms, ...
#' @return a list
.initializeXXX <- function(o, unconditional, residuals, resDen, se, fv.terms) {
  if (unconditional) { # Use Bayesian cov matrix including smoothing parameter uncertainty?
    if (is.null(o$gObj$Vc)) { 
      warning("Smoothness uncertainty corrected covariance not available") 
    } else { 
      o$gObj$Vp <- o$gObj$Vc 
    } 
  }
  
  w.resid <- NULL
  if (length(residuals) > 1) { # residuals supplied 
    if (length(residuals) == length(o$gObj$residuals)) { 
      w.resid <- residuals 
    } else { 
      warning("residuals argument to plot.gam is wrong length: ignored") 
    }
    partial.resids <- TRUE
  } else {
    partial.resids <- residuals
  } # use working residuals or none
  
  if (partial.resids || (resDen != "none")) { # Getting information needed for partial residuals
    if (is.null(w.resid)) { # produce working residuals if info available
      if (is.null(o$gObj$residuals) || is.null(o$gObj$weights)) {
        partial.resids <- FALSE
      } else {
        wr <- sqrt(o$gObj$weights)
        w.resid <- o$gObj$residuals * wr / mean(wr) # weighted working residuals
      }
    }
    if (partial.resids){ # get individual smooth effects
      fv.terms <- o$store$termsFit[ , o$store$np + o$ism]
      if(is.null(fv.terms)) { 
        fv.terms <- predict(o, type = "terms")
      }
    }
  }
  
  if (se) { # Sort out CI widths for 1D and 2D smooths
    if (is.numeric(se)) {
      se2.mult <- se1.mult <- se 
    } else {
      se1.mult <- 2
      se2.mult <- 1
    } 
    if (se1.mult < 0) {
      se1.mult <- 0
    }
    if (se2.mult < 0) {
      se2.mult <- 0 
    }
  } else {
    se1.mult <- se2.mult <- 1
  }
  if (se && o$gObj$Vp[1,1] < 0){  # Check that variances are actually available
    se <- FALSE
    warning("No variance estimates available")
  }
  
  ## Array giving the order of each parametric term
  order <- if (is.list(o$gObj$pterms)) {
    unlist(lapply(o$gObj$pterms, attr, "order")) 
  } else {
    attr(o$gObj$pterms, "order") 
  }
  
  return(list(
    o = o,
    order = order,
    w.resid = w.resid,
    se1.mult = se1.mult,
    se2.mult = se2.mult,
    se = se,
    fv.terms = fv.terms,
    partial.resids = partial.resids
  ))
}