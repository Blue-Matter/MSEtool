# A quantile plot

Plots quantiles and simulations for a stochastic time-series variable

## Usage

``` r
quantile_plot(
  datmat,
  xvals,
  p = c(0.05, 0.25, 0.5, 0.75, 0.95),
  tcol,
  ylim,
  sims = 1:3,
  refline = NA,
  dox = F,
  doy = F
)
```

## Arguments

- datmat:

  Matrix of real values with dimensions (simulation, year) (e.g.
  SB/SBMSY)

- xvals:

  Vector of numerical values of length ncol(datmat). The xaxis labels
  for datmat.

- p:

  Vector of quantiles five positions long. Defaults to
  c(0.05,0.25,0.5,0.75,0.95) so the 90% and 50% intervals with the
  median plotted in white.

- tcol:

  Color of shaded regions (transparent)

- ylim:

  Numerical vector of length 2, lower and upper limits for the yaxis

- sims:

  Vector of positive integers, the individual simulations to plot

- refline:

  Positive real number, a reference line to plot (on scale of y axis)

- dox:

  Logical, should the x axis labels be plotted.

- doy:

  Logical, should the y axis labels be plotted.

## Author

T. Carruthers
