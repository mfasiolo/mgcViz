
# **mgcViz**: visual tools for Generalized Additive Models

This R package offers visual tool for Generalized Additive Models
(GAMs). So far `mgcViz` provides: 

1. Layered smooth effect plots;  

2. New layered model checks;

3. Upgraded versions of the model checks provided by `mgcv`.

## Layered smooth effect plots

We work our way from the bottom up, starting with the smooth-specific plotting
methods and ending with the new `plot.gam` function, which wraps several plots
together.

#### Smooth-specific plots

Let's start with a simple example:

```R
library(mgcViz)
n  <- 1e3
x1 <- rnorm(n)
x2 <- rnorm(n)
dat <- data.frame("x1" = x1, "x2" = x2,
                  "y" = sin(x1) + 0.5 * x2^2 + pmax(x2, 0.2) * rnorm(n))
b <- bam(y ~ s(x1)+s(x2), data = dat, method = "fREML", discrete = TRUE)
```

Now we convert the fitted object to the `gamViz` class. Doing this allows to save
quite a lot of time when producing multiple plots using the same fitted GAM model.

```R
b <- getViz(b)
```

We then extract the same smooth component using the `sm` function we plot it.
The resulting `o` object contains, among other things, a `ggplot` object, which
allows us to add several layers.
```R
o <- plot( sm(b, 1) )
( o <- o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) )
```

Here we added only the fitted smooth and rugs on the x and y axes. Now we add in 
confidence lines at 1 and 5 standard deviations, partial residual points and we
change the theme to `ggplot2::theme_classic`.
```R
o + l_ciLine(mul = 1) + 
    l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
    l_points(shape = 19, size = 1, alpha = 0.1) + 
    wrapTheme(theme_classic())
```

Notice that `ggplot2` themes need to be wrapped using `wrapTheme`, before being 
added to the object. Functions such as `l_fitLine` or `l_rug` provide smooth-specific layers. So see all
the layers available for each smooth effect plot we can do:

```R
listLayers(o)
```

Similar methods exist for 2D smooth smooth effect plots, for instance if we fit:

```R
b <- bam(y ~ s(x1, x2), data = dat, method = "fREML", discrete = TRUE)
b <- getViz(b)
```

we can do

```R
plot(sm(b, 1)) + l_fitRaster() + l_fitContour() + l_points()
```

To convert a `gamViz` object back to its original form, we do:
```R
b <- getGam(b)
```

References
==========

-   Wickham, H. (2010) A layered grammar of graphics, Journal of
    Computational and Graphical Statistics, 19, 3–28.
