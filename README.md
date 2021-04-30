
# colourScaleR

<!-- badges: start -->
<!-- badges: end -->

`colourScaleR` provides a unified interface to [scico](https://github.com/thomasp85/scico), [viridis](https://github.com/sjmgarnier/viridis), & [Color Brewer](https://colorbrewer2.org) for creating colour scales

## Installation

You can install the development version of `colourScaleR` from [github](https://github.com/RichardJActon/colourScaleR) with:

``` r
remotes::install_github("RichardJActon/colourScaleR")
```

## Example

``` r
library(colourScaleR)

universal_colour_scaler(
	rnorm(10),
	type = "brewer",
	scale = "log",
	palette = "RdYlBu",
	verbose = TRUE,
	n_breaks = 10,
	mode = "scaled",
	direction = -1
)
```

