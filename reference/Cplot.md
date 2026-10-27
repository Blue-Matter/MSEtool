# Plot the median biomass and yield relative to last historical year

Compare median biomass and yield in first year and last 5 years of
projection

## Usage

``` r
Cplot(
  MSEobj,
  MPs = NA,
  lastYrs = 5,
  point.size = 2,
  lab.size = 4,
  axis.title.size = 12,
  axis.text.size = 10,
  legend.title.size = 12
)
```

## Arguments

- MSEobj:

  An object of class MSE

- MPs:

  Optional vector of MPs to plot

- lastYrs:

  Numeric. Last number of years to summarize results.

- point.size:

  Size of the points

- lab.size:

  Size of labels

- axis.title.size:

  Axis title size

- axis.text.size:

  Axis text size

- legend.title.size:

  Legend title size

## Examples

``` r
if (FALSE) { # \dontrun{
MSE <- runMSE()
Cplot(MSE)
} # }
```
