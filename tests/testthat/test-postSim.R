context("postSim")

test_that("postSim", {
  
  # Testing whether offsets lead to errors in multiple linear predictor case
  library(mgcViz)
  library(MASS)
  mcycle$o1 <- rep(0.01, 133)
  mcycle$o2 <- rep(0.05, 133)
  b <- gam(list(accel~s(times,k=20,bs="ad") + offset(o1), ~s(times)+ offset(o2)),
           data=mcycle,family=gaulss())
  
  expect_error(a<-postSim(o = b, nsim = 5, fun = mean), NA)
  expect_error(a<-postSim(o = b, nsim = 5, fun = mean, newdata = mcycle[1:20, ]), NA)
  expect_error(a<-postSim(o = b, nsim = 5, fun = mean, newdata = mcycle[1:20, ], 
                          offset = list(rep(1, 20), rep(0, 20))), NA)
  
  # Testing whether offsets lead to errors in single linear predictor case
  dat <- gamSim(1,n=400,dist="normal",scale=2)
  dat$off <- rep(0.1, 400)
  b <- gam(y~s(x0)+s(x1)+s(x2)+s(x3)+offset(off),data=dat)
  
  fn <- function(.x) .x[1:5]
  
  expect_error(a<-postSim(o = b, nsim = 5, fun = fn), NA)
  expect_error(a<-postSim(o = b, nsim = 5, fun = fn, newdata = dat[1:20, ]), NA)
  
  # Test whether it works with bam()
  dat <- gamSim(1,n=2500,dist="normal",scale=20)
  bs <- "cr"; k <- 12
  b <- bam(y ~ s(x0,bs=bs)+s(x1,bs=bs)+s(x2,bs=bs,k=k)+
               s(x3,bs=bs),data=dat)
  
  expect_error(a<-postSim(b, nsim = 10, fun = mean), NA)

})