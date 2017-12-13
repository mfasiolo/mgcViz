
[![Build Status](https://travis-ci.org/mfasiolo/mgcViz.svg?branch=master)](https://travis-ci.org/mfasiolo/mgcViz)

# **mgcViz**: visual tools for Generalized Additive Models

The `mgcViz` R package offers visual tools for Generalized Additive Models (GAMs). The visualizations provided by `mgcViz` differs from those implemented in `mgcv`, in that most of the plots are based on `ggplot2`'s powerful layering system. This has been implemented by wrapping several `ggplot2` layers and integrating them with computations specific to GAM models. Further, `mgcViz` uses binning and/or sub-sampling to produce plots that can scale to large datasets (n = O(10^7)), and offers a variety of new methods for visual model checking/selection.

See the vignette for an introduction to the following categories of visualizations: 

1. **smooth and parametric effect plots**: layered plots based on `ggplot2` and interactive 3d visualizations based on the `rgl` library;   

2. **model checks**: interactive QQ-plots, traditional residuals plots and layered residuals checks along one or two covariates;

3. **special plots**: differences-between-smooths plots in 1 or 2D and plotting multiple slices of multidimensional smooth effects.

