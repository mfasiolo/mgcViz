#'
#' Plotting one dimensional nested effects
#' 
#' @description This method should be used to plot smooth effects 
#'              of class \code{"si.smooth.1D"}.
#' @param x a smooth effect object.
#' @param inner if TRUE we are doing to plot the inner transformation, rather that then
#'              outer smooth effect. 
#' @param n number of grid points used to compute main effect and c.i. lines. 
#'          For a nice smooth plot this needs to be several times the estimated degrees of 
#'          freedom for the smooth.
#' @param xlim if supplied then this pair of numbers are used as the x limits for the plot.
#' @param ylim if supplied then this pair of numbers are used as the y limits for the plot.
#' @param maxpo maximum number of residuals points that will be used by layers such as
#'              \code{resRug()} and \code{resPoints()}. If number of datapoints > \code{maxpo},
#'              then a subsample of \code{maxpo} points will be taken.
#' @param trans monotonic function to apply to the smooth and residuals, before plotting.
#'              Monotonicity is not checked. 
#' @param ... currently unused.
#' @return An object of class \code{c("plotSmooth", "gg")}.
#' @name plot.nested1D
#' @rdname plot.nested1D
#' @export plot.nested1D
#' @export
#' 
plot.nested1D <- function(x, inner = FALSE, n = 100, xlim = NULL, ylim = NULL, maxpo = 1e4, trans = identity,  ...)  {
  
  if( inner ){
    # 1) Prepare data
    P <- .prepareInnerNested(o = x, n = n, xlim = xlim, ylim = ylim, ...)
    
    out <- .plot.inner.nested.smooth.1D(P = P, trans = trans, maxpo = maxpo)
    
    # 【新增】针对双重嵌套，如果返回的是图表List，直接 return 即可
    if(!is.null(P) && P$type == "si_nexpsm"){
      return(out)
    }
    
  } else {
    # 1) Prepare data
    P <- .prepareOuterNested(o = x, n = n, xlim = xlim, ...)
    
    # 2) Produce output object
    out <- .plot.outer.nested.1D(x = P$smooth, P = P, trans = trans, maxpo = maxpo)
  }
  
  class(out) <- c("plotSmooth", "gg")
  
  return(out)
}

########################
#' @noRd
.plot.outer.nested.1D <- function(x, P, trans, maxpo) {
  
  .dat <- list()
  
  if ( !is.null(P$raw) ) {
    # Construct data.frame of partial residuals
    res <- data.frame("x" = as.vector(P$raw))
    
    # Exclude residuals falling outside boundaries
    .dat$res <- res[res$x >= P$xlim[1] & res$x <= P$xlim[2], , drop = FALSE]
    
    # Sample if too many points (> maxpo)  
    nres <- nrow( .dat$res )
    .dat$res$sub <- if(nres > maxpo) { 
      sample( c(rep(T, maxpo), rep(F, nres-maxpo)) )
    } else { 
      rep(T, nres) 
    }
  }
  
  .dat$fit <- data.frame(x = P$x, y = P$fit, ty = trans(P$fit), se = P$se)
  .dat$misc <- list(trans = trans)
  
  .pl <- ggplot(data = .dat$fit, mapping = aes(x = x, y = y)) + 
    labs(title = P$main, x = P$xlab, y = P$ylab) + theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  return( list("ggObj" = .pl, "data" = .dat, type = "1D") )
}

########################
#' @noRd
.plot.inner.nested.smooth.1D <- function(P, trans, maxpo) {
  
  # 空值检查 (提早返回是合理的)
  if( is.null(P) ) { return(NULL) }
  
  # =========================================================
  # 根据不同的嵌套模型类型 (type) 构建内层 ggplot 对象
  # =========================================================
  if( P$type == "si_nexpsm" ){
    # ---------------------------------------------------------
    # 1. 双重嵌套模型 (si_nexpsm) - 返回 3 个独立图表的 List
    # ---------------------------------------------------------
    build_coef_plot <- function(subP) {
      .dat <- list()
      .dat$fit <- data.frame("x"  = as.factor(subP$x),
                             "y"  = unname(subP$fit),
                             "ty" = trans(unname(subP$fit)),
                             "se" = unname(subP$se))
      .dat$misc <- list("trans" = trans)
      
      .pl <- ggplot(data = .dat$fit, aes("x" = x, "y" = ty)) +
        labs(title = subP$main, x = subP$xlab, y = subP$ylab) +
        theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      return(structure(list("ggObj" = .pl, "data" = .dat, "type" = c("si", "Factor")), 
                       class = c("plotSmooth", "gg")))
    }
    
    build_xa_plot <- function(subP) {
      .dat <- list()
      .dat$fit <- data.frame("x" = subP$x, 
                             "y" = subP$fit, 
                             "ty" = trans(subP$fit))
      .dat$misc <- list("trans" = trans)
      
      .pl <- ggplot(data = .dat$fit, aes(x = x, y = y)) + 
        labs(title = subP$main, x = subP$xlab, y = subP$ylab) + 
        theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      return(structure(list("ggObj" = .pl, "data" = .dat, "type" = "1D"), 
                       class = c("plotSmooth", "gg")))
    }
    
    out <- list(
      plot_si   = build_coef_plot(P$si),
      plot_nexp = build_coef_plot(P$nexp),
      plot_xa   = build_xa_plot(P$xa)
    )
    
  } else if( P$type == "nexpsm" ){
    # ---------------------------------------------------------
    # 2. 指数平滑模型 (nexpsm) - 调用通用的 1D 画图函数
    # ---------------------------------------------------------
    out <- .plot.mgcv.smooth.1D(x = NULL, P = P, trans = trans, maxpo = maxpo)
    
  } else if( P$type == "mgks" ){
    # ---------------------------------------------------------
    # 3. 二维距离核平滑 (mgks) - 包含残差子采样逻辑
    # ---------------------------------------------------------
    .dat <- list()
    .dat$fit <- data.frame("z"  = drop( P$fit ),
                           "tz" = drop( trans(P$fit) ),
                           "x"  = rep(P$x, length(P$fit) / length(P$x)),
                           "y"  = rep(P$y, each = length(P$fit) / length(P$x)),
                           "se" = P$se)
    
    P$raw <- data.frame(z = P$p.resid, x = P$X0[ , 1], y = P$X0[ , 2])
    if( !is.null(P$raw) ){
      .dat$res <- P$raw[P$raw$x >= P$xlim[1] & P$raw$x <= P$xlim[2] &
                          P$raw$y >= P$ylim[1] & P$raw$y <= P$ylim[2] , , drop = FALSE]
      
      nres <- nrow( .dat$res )
      .dat$res$sub <- if(nres > maxpo) {
        sample( c(rep(TRUE, maxpo), rep(FALSE, nres-maxpo)) )
      } else {
        rep(TRUE, nres)
      }
    }
    
    .dat$misc <- list("trans" = trans)
    
    .pl <- ggplot(data = .dat$fit, aes(x = x, y = y, z = z)) +
      labs(title = P$main, x = P$xlab, y = P$ylab) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    out <- list("ggObj" = .pl, "data" = .dat, "type" = c("mgks", "2D"))
    
  } else if( P$type == "si" ){
    # ---------------------------------------------------------
    # 4. 单指数模型 (si) - 默认绘制因子图
    # ---------------------------------------------------------
    .dat <- list()
    .dat$fit <- data.frame("x"  = as.factor(P$x),
                           "y"  = unname(P$fit),
                           "ty" = trans( unname(P$fit) ),
                           "se" = unname(P$se) )
    .dat$misc <- list("trans" = trans)
    
    .pl <- ggplot(data = .dat$fit, aes("x" = x, "y" = ty)) +
      labs(title = P$main, x = P$xlab, y = P$ylab) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
    
    out <- structure(list("ggObj" = .pl, "data" = .dat, "type" = c("si", "Factor")), 
                     class = c("plotSmooth",  "gg"))
    
  } else {
    # ---------------------------------------------------------
    # 5. 兜底保护
    # ---------------------------------------------------------
    stop("Unrecognized smooth effect type in .plot.inner.nested.smooth.1D: ", P$type)
  }
  
  # 统一返回
  return(out)
}
