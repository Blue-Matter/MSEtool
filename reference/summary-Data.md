# Summary of Data object

Summary of Data object

## Usage

``` r
# S4 method for class 'Data'
summary(
  object,
  wait = TRUE,
  x = 1,
  plots = "all",
  rmd = FALSE,
  head = "##",
  tplot = 25
)
```

## Arguments

- object:

  An object of class Data

- wait:

  Logical. Wait for key press before next plot?

- x:

  iteration number for the Data object.

- plots:

  Character. What plots to show? `all`, `TS`, `CAA`, `CAL`, `PD` for all
  plots, time-series, catch-at-age, catch-at-length, and probability
  distributions respectively

- rmd:

  Logical. Used in a rmd file?

- head:

  Character. Heading for rmd file. Default is '##' (second level
  heading)

- tplot:

  Integer. Number of plots per page. Default 25
