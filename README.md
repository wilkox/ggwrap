[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ggwrap)](https://cran.r-project.org/package=ggwrap)
[![Lifecycle: deprecated](https://img.shields.io/badge/lifecycle-deprecated-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#deprecated)

# This package is no longer maintained

Use [`ggbreak::scale_wrap()`](https://cran.r-project.org/web/packages/ggbreak/vignettes/ggbreak.html#wrap-plot) instead.

# ggwrap

‘ggwrap’ wraps a ‘ggplot2’ plot over multiple rows, to make plots with
long x axes easier to read.

# Installation

The development version of ‘ggwrap’ can be installed from GitHub:

```r
devtools::install_github("wilkox/ggwrap")
```

# Usage

The `ggwrap` function takes two arguments: a ‘ggplot2’ plot, and the
number of rows over which the plot should be wrapped.

Here’s a simple plot.

```r
library(ggplot2)
library(ggwrap)
plot <- ggplot(economics, aes(x = date, y = unemploy, colour = uempmed)) +
  geom_line()
plot
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

We can use `ggwrap` to wrap the plot over four rows.

```r
ggwrap(plot, 4)
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->
