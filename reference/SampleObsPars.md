# Sample Observation Parameters

Sample Observation Parameters

## Usage

``` r
SampleObsPars(
  Obs,
  nsim = NULL,
  cpars = NULL,
  Stock = NULL,
  nyears = NULL,
  proyears = NULL
)
```

## Arguments

- Obs:

  An object of class 'Obs' or class 'OM'

- nsim:

  Number of simulations. Ignored if 'Obs' is class 'OM'

- cpars:

  Optional named list of custom parameters. Ignored if 'OM' is class
  'OM'

- Stock:

  An object of class 'Stock' or a list of sampled Stock parameters. Not
  essential.

- nyears:

  Number of historical years.

- proyears:

  Number of projection years. Not essential.

## Value

A named list of sampled Observation parameters
