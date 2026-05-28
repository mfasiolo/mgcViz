# These checks target this GitHub issue:
# https://github.com/mfasiolo/mgcViz/issues/84

test_that("l_gridCheck2D shows all factor levels for factor/numeric and factor/factor checks", {
  
  set.seed(0)
  
  f0 <- function(x) 2 * sin(pi * x)
  f1 <- function(x, a = 2, b = -1) exp(a * x) + b
  f2 <- function(x) {
    0.2 * x^11 * (10 * (1 - x))^6 +
      10 * (10 * x)^3 * (1 - x)^10
  }
  
  n <- 2000
  nf <- 5
  
  dat <- data.frame(
    fac = factor(sample(letters[1:nf], n, replace = TRUE)),
    fac2 = factor(sample(LETTERS[1:nf], n, replace = TRUE)),
    x0 = runif(n),
    x1 = runif(n),
    x2 = runif(n)
  )
  
  dat$y <- with(dat, f0(x0) + f1(x1) + f2(x2) + rnorm(n) * 2)
  
  b <- gam(
    y ~ fac + fac2 + s(x0) + s(x1) + s(x2),
    data = dat
  )
  
  b <- getViz(b, nsim = 50)
  
  #### [1] Factor/numeric check
  p <- check2D(b, "fac", "x1") +
    l_gridCheck2D()

  print(p + ggplot2::ggtitle(paste0("The grid must have ", nf, " bins along the factor axis!")))
  
  gb <- ggplot2::ggplot_build(p$ggObj)
  
  # First layer should be the stat_summary_2d heatmap layer.
  d <- gb$data[[1]]
  
  # Count non-empty factor-direction bin centres.
  expect_equal(length(unique(d$x[!is.na(d$value)])), nf)
  
  #### [2] Factor/factor check
  p <- check2D(b, "fac", "fac2") +
    l_gridCheck2D()
  
  print(p + ggplot2::ggtitle(paste0("The grid must have ", nf, " bins along both axes!")))
  
  gb <- ggplot2::ggplot_build(p$ggObj)
  
  # First layer should be the stat_summary_2d heatmap layer.
  d <- gb$data[[1]]
  
  # Count non-empty factor-direction bin centres.
  expect_equal(length(unique(d$x[!is.na(d$value)])), nf)
  
  expect_equal(length(unique(d$y[!is.na(d$value)])), nf)
})

test_that("check2D preserves supplied factor level order", {
  
  library(mgcv)
  library(mgcViz)
  
  set.seed(566)
  
  n <- 1000
  
  X <- data.frame(
    x1 = rnorm(n, 0.5, 0.5),
    x2 = rnorm(n, 1.5, 1),
    fac = factor(
      sample(c(2, 6, 10, 14, 18, 22), n, replace = TRUE),
      levels = c("2", "6", "10", "14", "18", "22")
    )
  )
  
  X$y <- (1 - X$x1)^2 + 100 * (X$x2 - X$x1^2)^2 + rnorm(n, 0, 2)
  
  b <- gam(y ~ te(x1, x2, k = 5), data = X)
  b <- getViz(b, nsim = 20)
  
  ## Factor supplied directly
  ck1 <- check2D(b, x1 = X$fac, x2 = "x2")
  
  expect_equal(
    levels(ck1$data$res$x),
    c("2", "6", "10", "14", "18", "22")
  )
  
  ck1_1d <- check1D(b, x = X$fac)
  
  expect_equal(
    levels(ck1_1d$data$res$x),
    c("2", "6", "10", "14", "18", "22")
  )
  
  ## Factor supplied by name, if present in stored data
  b2 <- gam(y ~ te(x1, x2, k = 5), data = X)
  b2 <- getViz(b2, nsim = 20, newdata = X)
  
  ck2 <- check2D(b2, x1 = "fac", x2 = "x2")
  
  expect_equal(
    levels(ck2$data$res$x),
    c("2", "6", "10", "14", "18", "22")
  )
  
  ck2_1d <- check1D(b2, x = X$fac)
  
  expect_equal(
    levels(ck2_1d$data$res$x),
    c("2", "6", "10", "14", "18", "22")
  )
})
