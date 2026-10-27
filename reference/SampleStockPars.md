# Sample Stock parameters

Sample Stock parameters

## Usage

``` r
SampleStockPars(
  Stock,
  nsim = 48,
  nyears = 80,
  proyears = 50,
  cpars = NULL,
  msg = TRUE
)
```

## Arguments

- Stock:

  An object of class 'Stock' or class 'OM'

- nsim:

  Number of simulations. Ignored if 'Stock' is class 'OM'

- nyears:

  Number of historical years. Ignored if 'Stock' is class 'OM'

- proyears:

  Number of projection years. Ignored if 'Stock' is class 'OM'

- cpars:

  Optional named list of custom parameters. Ignored if 'Stock' is class
  'OM'

- msg:

  logical. Warning message for M values?

## Value

A named list of sampled Stock parameters
