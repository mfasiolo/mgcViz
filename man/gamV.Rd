% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bamV.R, R/gamV.R
\name{bamV}
\alias{bamV}
\alias{gamV}
\title{Fit a GAM model and get a gamViz object}
\usage{
bamV(
  formula,
  family = gaussian(),
  data = list(),
  method = "fREML",
  aGam = list(),
  aViz = list()
)

gamV(
  formula,
  family = gaussian(),
  data = list(),
  method = "REML",
  aGam = list(),
  aViz = list()
)
}
\arguments{
\item{formula, family, data, method}{same arguments as in \link[mgcv:gam]{mgcv::gam} or \link[mgcv:bam]{mgcv::bam}.}

\item{aGam}{list of further arguments to be passed to \link[mgcv:gam]{mgcv::gam} or \link[mgcv:bam]{mgcv::bam}.}

\item{aViz}{list of arguments to be passed to \link{getViz}.}
}
\value{
An object of class "gamViz" which can, for instance, be plotted using \link{plot.gamViz}.
}
\description{
These are wrapper that fits a GAM model using \link[mgcv:gam]{mgcv::gam} or \link[mgcv:bam]{mgcv::bam} and
converts it to a \code{gamViz} object using the \link{getViz} function.
It is essentially a shortcut.
}
\examples{
##### gam example
# Simulate data
library(mgcViz)
set.seed(2) ## simulate some data...
dat <- gamSim(1,n=1000,dist="normal",scale=2)

# Fit GAM and get gamViz object
b <- gamV(y~s(x0)+s(x1, x2)+s(x3), data = dat, 
          aGam = list(scale = 2), aViz = list("nsim" = 20))

# This is equivalent to doing
# 1. Fit GAM
# b <- gam(y~s(x0)+s(x1, x2)+s(x3), data=dat, method="REML", scale = 2)
# 2. Convert to gamViz object
# b <- getViz(b, nsim = 20)

# Either way, we plot first and third effects by doing
print(plot(b, select = c(1, 3)), pages = 1)

##### bam example
# Simulate data
library(mgcViz)
set.seed(2) ## simulate some data...
dat <- gamSim(1,n=2000,dist="normal",scale=2)

# Fit using bam() and get gamViz object
b <- bamV(y~s(x0)+s(x1, x2)+s(x3), data = dat, 
          aGam = list(discrete = TRUE), aViz = list("nsim" = 0))
          
# Either way, we plot first and third effects by doing
print(plot(b, select = c(2)), pages = 1)
}
