# A fairly tidy time-series quantile plot

A fairly tidy time-series quantile plot

## Usage

``` r
plotquant(
  x,
  p = c(0.05, 0.25, 0.75, 0.95),
  yrs,
  qcol,
  lcol,
  addline = T,
  ablines = NA
)
```

## Arguments

- x:

  Matrix. A time series quantity `[simulation, year]`

- p:

  Numeric vector. The percentiles that are plotted (LB2,LB1,UB1,UB2).
  LB2 and UB2 are the outer percentiles, LB1 and UB1 are the inner
  percentiles.

- yrs:

  Numeric vector. The years corresponding to the indexing of x

- qcol:

  Character, color. The color of the inner percentile range

- lcol:

  Character, color. The color of the outer percentile range.

- addline:

  Logical. Should two individual simulations be added to the percentile
  plots?

- ablines:

  Numeric vector. Horizontal lines to be added to the plot.

## Author

T.Carruthers
