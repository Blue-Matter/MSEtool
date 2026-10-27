# Linear interpolation of a y value at level xlev based on a vector x and y

Linear interpolation of a y value at level xlev based on a vector x and
y

## Usage

``` r
LinInterp(x, y, xlev, ascending = FALSE, zeroint = FALSE)
```

## Arguments

- x:

  A vector of x values

- y:

  A vector of y values (identical length to x)

- xlev:

  A the target level of x from which to guess y. Can be either a numeric
  or vector.

- ascending:

  Are the the x values supposed to be ordered before interpolation

- zeroint:

  is there a zero-zero x-y intercept?

## Details

As of version 3.2, this function uses
[`stats::approx`](https://rdrr.io/r/stats/approxfun.html)

## Author

T. Carruthers
