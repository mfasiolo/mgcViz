context("plot.mgcv.smooth ")

test_that("plot.mgcv.smooth", {
  
 library(mgcViz); library(plyr); library(gridExtra)
 set.seed(2) ## simulate some data...
 dat <- gamSim(1,n=200,dist="normal",scale=2)
 x <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
 
 combs <- as.matrix(expand.grid(c(F,T),c(F,T),c(F,T),c(F,T)))
 
 plts <- alply(combs, 1, function(.inp) 
   plot(x, residuals = .inp[1], scale = .inp[2], se = .inp[3], rug = .inp[4], select = 1, draw = F)[[1]])
 
 do.call(grid.arrange, plts)

})
