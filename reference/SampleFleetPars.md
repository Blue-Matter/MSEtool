# Sample Fleet Parameters

Sample Fleet Parameters

## Usage

``` r
SampleFleetPars(
  Fleet,
  Stock = NULL,
  nsim = NULL,
  nyears = NULL,
  proyears = NULL,
  cpars = NULL,
  msg = TRUE
)
```

## Arguments

- Fleet:

  An object of class 'Fleet' or class 'OM'

- Stock:

  An object of class 'Stock' or a list of sampled Stock parameters.
  Ignored if 'Fleet' is class 'OM'

- nsim:

  Number of simulations. Ignored if 'Fleet' is class 'OM'

- nyears:

  Number of historical years. Ignored if 'Fleet' is class 'OM'

- proyears:

  Number of projection years. Ignored if 'Fleet' is class 'OM'

- cpars:

  Optional named list of custom parameters. Ignored if 'Fleet' is class
  'OM'

- msg:

  Logical. Print messages?

## Value

A named list of sampled Fleet parameters
