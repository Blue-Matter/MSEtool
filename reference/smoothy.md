# General purpose polynomial smoother

Polynomial smoother (no gradient prediction) applied to a vector that
can include NA values. Intended to be rapid for use in management
procedures

## Usage

``` r
smoothy(xx, plot = F, enp_mult, plotname = "", xlab = "x", ylab = "y", x = NA)
```

## Arguments

- xx:

  Vector of real numbers, data to be smoothed.

- plot:

  Logical, should the 'fit' of the smoother be plotted?

- enp_mult:

  Fraction, effective number of parameters multiplier. The smoother
  parameter number is length(xx) x enp_mult. So higher values of
  enp_mult means less smoothing (more parameters).

- plotname:

  Character, in case you want to put a label on the plot (plot = T).

- xlab:

  Character, in case you want an xaxis label on the plot (plot = T)

- ylab:

  Character, in case you want a yaxis label on the plot (plot = T)

- x:

  Numeric vector same length as xx, in case you want to have a custom
  xaxis (e.g. years)

## Author

T. Carruthers
