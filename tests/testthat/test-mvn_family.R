test_that("mvn_family", {
  
  library(mgcViz)
  # Simulate some data
  V <- matrix(c(2,1,1,2),2,2)
  f0 <- function(x) 2 * sin(pi * x)
  f1 <- function(x) exp(2 * x)
  f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 * 
    (10 * x)^3 * (1 - x)^10
  n <- 3000
  x0 <- runif(n);x1 <- runif(n);
  x2 <- runif(n);x3 <- runif(n)
  y <- matrix(0,n,2)
  for (i in 1:n) {
    mu <- c(f0(x0[i])+f1(x1[i]),f2(x2[i]))
    y[i,] <- rmvn(1,mu,V)
  }
  dat <- data.frame(y0=y[,1],y1=y[,2],x0=x0,x1=x1,x2=x2,x3=x3)
  
  # Fit model
  b <- gamV(list(y0~s(x0)+s(x1),y1~s(x2)+s(x3)),family=mvn(d=2),data=dat)
  b <- getViz(b, nsim = 10)
  
  # Check in 0, 1 and 2 dimensions
  expect_error(print(check0D(b) + l_hist() + l_rug()), NA)

  expect_error(print(check1D(b, x0) + l_gridCheck1D()), NA)
  expect_error(print(check1D(b, x0, trans = function(.x) .x[ , 2]) + l_gridCheck1D()), NA)
  
  expect_error(print(check2D(b, x1 = "x0", x2 = "x1") + 
               l_gridCheck2D(gridFun = sd) + l_rug() + l_points()), NA)

})
