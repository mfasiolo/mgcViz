
##########
# Internal method
#
.prepareInnerNested <- function(o, n, xlim, ylim = NULL, ...){
  
  if(!exists("expsmooth") || !exists("mgks") ){
    expsmooth <- mgks <- function(x){}
    stop("Please install the gamFactory package.")
  }
  
  gObj <- o$gObj
  sm <- gObj$smooth[[ o$ism ]]
  
  si <- sm$xt$si
  alpha <- si$alpha 
  B <- si$B
  
  da <- length( alpha )
  prange <- (sm$first.para:sm$last.para)[1:da]
  type <- class(o)[1]
  
  # Remove scale parameter of inner transformation
  if( type %in% c("mgks", "nexpsm")){
    prange <- prange[-1]
    da <- da-1
    alpha <- alpha[-1]
    if(is.null(B)){
      B <- diag(1, nrow = length(alpha))
    }
  }
  
  Va <- gObj$Vp[prange, prange, drop = FALSE]
  
  # =========================================================
  # 根据不同的嵌套模型类型 (type) 准备内层绘图数据
  # =========================================================
  if( type == "si_nexpsm" ){
    # ---------------------------------------------------------
    # 1. 双重嵌套模型 (si_nexpsm)
    # ---------------------------------------------------------
    # 获取该平滑项对应的所有参数的协方差矩阵 (内层+外层，与 jacobian 维度匹配)
    prange_all <- sm$first.para:sm$last.para
    Va_all <- gObj$Vp[prange_all, prange_all, drop = FALSE]
    
    n_nexp <- si$n_nexp
    n_si <- si$n_si
    
    # 准备 alpha_nexp
    idx_nexp <- 1:n_nexp
    alpha_nexp <- si$alpha_nexp
    Va_nexp <- Va_all[idx_nexp, idx_nexp, drop = FALSE]
    if(!is.null(si$B_nexp)){
      alpha_nexp <- drop(si$B_nexp %*% alpha_nexp)
      Va_nexp <- si$B_nexp %*% Va_nexp %*% t(si$B_nexp)
    }
    se_nexp <- sqrt(pmax(0, diag(Va_nexp)))
    
    # 准备 alpha_si
    idx_si <- (n_nexp + 1):(n_nexp + n_si)
    alpha_si <- si$alpha_si
    Va_si <- Va_all[idx_si, idx_si, drop = FALSE]
    if(!is.null(si$B_si)){
      alpha_si <- drop(si$B_si %*% alpha_si)
      Va_si <- si$B_si %*% Va_si %*% t(si$B_si)
    }
    se_si <- sqrt(pmax(0, diag(Va_si)))
    
    # 准备 xa 的轨迹
    xa <- sm$xt$xa
    times <- 1:length(xa)
    
    out <- list(
      type = "si_nexpsm",
      main = NULL,
      si = list(fit = alpha_si, x = 1:length(alpha_si), se = se_si,
                xlab = "SI Index", ylab = "Alpha SI Weights", main = "Single Index Weights"),
      nexp = list(fit = alpha_nexp, x = 1:length(alpha_nexp), se = se_nexp,
                  xlab = "NEXP Index", ylab = "Alpha NEXP Weights", main = "Exp Smooth Parameters"),
      xa = list(fit = xa, x = times,
                xlab = "Index(Time)", ylab = "s_t", main = "After Smooth")
    )
    
  } else if( type == "nexpsm" ){
    # ---------------------------------------------------------
    # 2. 指数平滑模型 (nexpsm)
    # ---------------------------------------------------------
    inner <- expsmooth(y = si$x, Xi = si$X, beta = alpha, deriv = 1)
    fit <- inner$d0
    Jac <- inner$d1
    if( !is.null(si$times) ){
      fit <- fit[1:max(si$times)]
      Jac <- Jac[1:max(si$times), ]
    }
    nobs <- length(fit)
    se <- sqrt(pmax(0, rowSums((Jac %*% Va) * Jac)))
    edf   <- sum(gObj$edf[prange])
    ylabel <- .subEDF(paste0("expsm(", sm$term, ")"), edf)
    xlabel <- "Index"
    
    if( !is.null(xlim) ) {
      xlim <- sort(xlim)
      xlim[1] <- max(xlim[1], 1)
      xlim[2] <- min(xlim[2], nobs)
      ii <- which(1:nobs >= xlim[1] & 1:nobs <= xlim[2])
      nobs <- length(ii)
    } else {
      xlim <- c(1, nobs)
      ii <- 1:nobs
    }
    
    out <- list("fit" = fit[ii], "x" = ii, "se" = se[ii],
                "p.resid" = si$x[ii], "raw" = ii, 
                "xlim" = xlim, 
                xlab = xlabel, ylab = ylabel, main = NULL, type = "nexpsm")
    
  } else if( type == "si" ) {
    # ---------------------------------------------------------
    # 3. 单指数模型 (si)
    # ---------------------------------------------------------
    a0 <- si$a0
    if( is.null(a0) ){
      a0 <- alpha * 0
    }
    alpha <- drop(B %*% (alpha + a0))
    Va <- B %*% Va %*% t(B)
    se <- sqrt(pmax(0, diag(Va)))
    edf   <- sum(gObj$edf[prange])
    ylabel <- .subEDF(paste0("Inner_coef(", sm$term, ")"), edf)
    xlabel <- "Index"
    
    out <- list("fit" = alpha, "x" = 1:da, "se" = se,
                xlab = xlabel, ylab = ylabel, main = NULL, type = "si")
    
  } else {
    # ---------------------------------------------------------
    # 4. 未知类型兜底 / 保留给未来激活的 mgks
    # ---------------------------------------------------------
    # NOT CLEAR HOW TO DO THIS WITH DISTANCEs
    # if( type == "mgks" ){
    #   d <- ncol(si$X0)
    #   if( d != 2 ){ return( NULL ) }
    #   
    #   if( !is.null(xlim) ) xlim <- sort(xlim) else xlim <- range(si$X[ , 1])
    #   if( !is.null(ylim) ) ylim <- sort(ylim) else ylim <- range(si$X[ , 2])
    #   
    #   xx <- rep(seq(xlim[1], xlim[2], length.out = n), n)
    #   yy <- rep(seq(ylim[1], ylim[2], length.out = n), rep(n, n))
    #   X <- cbind(xx, yy)
    #   
    #   si$x <- as.matrix(si$x)
    #   if( ncol(si$x) > 1 ){ si$x <- colMeans(si$x) }
    #   
    #   inner <- mgks(y = si$x, X = X, X0 = si$X0, beta = alpha[-1], deriv = 1)
    #   fit <- inner$d0
    #   Jac <- inner$d1
    #   se <- sqrt(pmax(0, rowSums((Jac %*% Va[-1, -1, drop = FALSE]) * Jac)))
    #   edf   <- sum(gObj$edf[prange[-1]])
    #   
    #   mainlab <- .subEDF(paste0("mgks(", sm$term, ")"), edf)
    #   out <- list("fit" = fit, "X" = si$X, "se" = se, "x" = xx, "y" = yy,
    #               "p.resid" = si$x, "X0" = si$X0, 
    #               "xlim" = xlim, "ylim" = ylim,
    #               "xlab" = "X[ , 1]", "ylab" = "X[ , 2]", "main" = mainlab, type = "mgks")
    # } else {
    stop("Unrecognized smooth effect type in .prepareInnerNested: ", type)
    # }
  }
  
  return(out)
  
}