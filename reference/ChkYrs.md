# Check the years to summarize performance

Check the years to summarize performance

## Usage

``` r
ChkYrs(Yrs, MSEobj)
```

## Arguments

- Yrs:

  Numeric vector of length 2 with year indices to summarize performance.
  If NULL, the performance is summarized over all projection years.
  `Yrs` can also be length one, in which case if it is positive it is
  the first `Yrs` and if negative the last `Yrs` of the projection
  period.

- MSEobj:

  An object of class `MSE`

## Value

A numeric vector of length 2 with year indices to summarize performance

## Examples

``` r
if (FALSE) { # \dontrun{
MSE <- runMSE()
ChkYrs(NULL, MSE) # returns c(1, MSE@proyears)
ChkYrs(c(2,5), MSE) # returns c(2,5)
ChkYrs(c(70,80), MSE) # returns c(MSE@proyears-10,MSE@proyears)
ChkYrs(5, MSE) # returns c(1,5)
ChkYrs(-5, MSE) # returns c(46,50)
} # }
```
