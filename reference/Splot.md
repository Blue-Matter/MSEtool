# Standard MSE projection plot

Plots projections of F/FMSY, SB/SBMSY and Yield

## Usage

``` r
Splot(MSEobj, MPs = 5, p = c(0.05, 0.25, 0.5, 0.75, 0.95))
```

## Arguments

- MSEobj:

  Object of class 'MSE' from runMSE() or Project()

- MPs:

  Either a positive integer (the first MPs number of MPs to plot), a
  character vector (the names of the MPs to plot), or an integer vector
  (the index of the MPs to plot)

- p:

  Vector of quantiles five positions long. Defaults to
  c(0.05,0.25,0.5,0.75,0.95) so the 90% and 50% intervals with the
  median plotted in white.

## Author

T. Carruthers
