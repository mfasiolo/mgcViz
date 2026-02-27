context("plot.nested1D ")

test_that("plot.nested1D", {
  library(mgcViz)
  library(gamFactory)
  
  set.seed(2)
  n <- 200
  X_si <- matrix(rnorm(n * 3), n, 3)
  X_nexp <- matrix(rnorm(n * 3), n, 3)
  X_all <- matrix(rnorm(n * 5), n, 5)
  colnames(X_nexp) <- c("y", "x", "x")
  
  y <- sin(X_si %*% c(0.5, 0.3, 0.2)) +
    sin(X_nexp %*% c(0.4, 0.1, -0.1)) +
    sin(X_all %*% c(0.5, 0.3, 0.2, 0.1, -0.1)) +
    rnorm(n, sd = 0.2)
  
  dat <- data.frame(y = y)
  dat$X_si <- X_si
  dat$X_nexp <- X_nexp
  dat$X_all <- X_all
  
  fit <- gam_nl(
    list(
      y ~ s_nest(X_si, trans = trans_linear()) +
        s_nest(X_nexp, trans = trans_nexpsm()) +
        s_nest(X_all, k = 10, trans = trans_linear_nexpsm(n_si = 3, n_nexp = 2)),
      ~ 1
    ),
    family = fam_gaussian(),
    data = dat,
    optimizer = "efs"
  )
  
  viz <- getViz(fit)
  
  ## extract smooths
  # 1: si, 2: nexp, 3: si_nexp
  test_eff_si <- sm(viz, 1)
  test_eff_nexp <- sm(viz, 2)
  test_eff_si_nexp <- sm(viz, 3)
  
  ## test si effects
  expect_error(plot.nested1D(test_eff_si, inner = TRUE, n = 1000), NA)
  expect_error(plot.nested1D(test_eff_si, inner = FALSE, n = 1000), NA)
  
  ## test nexp effects
  expect_error(plot.nested1D(test_eff_nexp, inner = TRUE, n = 1000), NA)
  expect_error(plot.nested1D(test_eff_nexp, inner = FALSE, n = 1000), NA)
  
  ## test si_nexp effects
  expect_error(plot.nested1D(test_eff_si_nexp, inner = TRUE, n = 1000), NA)
  expect_error(plot.nested1D(
    test_eff_si_nexp,
    inner = TRUE,
    smooth = TRUE,
    n = 1000
  ),
  NA)
  expect_error(plot.nested1D(test_eff_si_nexp, inner = FALSE, n = 1000), NA)
  
  ## test global plot
  expect_error(plot(
    viz,
    pages = 1,
    inner = TRUE,
    n = 1000
  ), NA)
  
})