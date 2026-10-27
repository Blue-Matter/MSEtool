# Internal function for checking that the `OM@cpars` is formatted correctly

Internal function for checking that the `OM@cpars` is formatted
correctly

## Usage

``` r
cparscheck(cpars)
```

## Arguments

- cpars:

  a list of model parameters to be sampled (single parameters are a
  vector `nsim` long, first dimension of matrices and arrays must be
  `nsim`)

## Value

either an error and the length of the first dimension of the various
cpars list items or passes and returns the number of simulations in
`cpars`

## Author

T. Carruthers
