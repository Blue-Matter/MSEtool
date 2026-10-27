# Standard plot for an object of class MMSE (multi MSE)

Plot the projected biomass, fishing, mortality rate and yield for all
stocks and MPs

## Usage

``` r
# S3 method for class 'MMSE'
plot(
  x = NULL,
  maxcol = 6,
  qcol = rgb(0.4, 0.8, 0.95),
  lcol = "dodgerblue4",
  quants = c(0.05, 0.25, 0.75, 0.95),
  curyr = 2018,
  addline = FALSE,
  ...
)
```

## Arguments

- x:

  Object of class
  [MMSE](https://msetool.openmse.com/reference/MMSE-class.md). A
  Multi-OM object created by `multiMSE(MOM, ...)`

- maxcol:

  Integer. The maximum number of columns (MPs) to be plotted in each
  plot

- qcol:

  Character, color. The color of the inner percentile range

- lcol:

  Character, color. The color of the outer percentile range.

- quants:

  Numeric vector. The percentiles that are plotted (LB2,LB1,UB1,UB2).
  LB2 and UB2 are the outer percentiles, LB1 and UB1 are the inner
  percentiles.

- curyr:

  Integer. The current year from which projections start.

- addline:

  Logical. Should two individual simulations be added to the percentile
  plots?

- ...:

  Not used

## Author

T.Carruthers
