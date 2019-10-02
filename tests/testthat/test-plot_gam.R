context("plot.gam ")

test_that("plot.gam", {
  
 library(mgcViz)
 set.seed(2) ## simulate some data...
 dat <- gamSim(1,n=200,dist="normal",scale=2)
 x <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
 
 expect_error(plot(x, pages = 1), NA)
 
})
