# Plot Data object

Creates plots of the Data object in the R console. Wrapper for
`summary(Data)`

## Usage

``` r
# S3 method for class 'Data'
plot(
  x,
  wait = TRUE,
  i = 1,
  plots = "all",
  rmd = FALSE,
  head = "##",
  tplot = 25,
  ...
)
```

## Arguments

- x:

  An object of class Data

- wait:

  Logical. Wait for key press before next plot?

- i:

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

- ...:

  Not used
