# Check Convergence

Have I undertaken enough simulations (nsim)? Has my MSE converged on
stable (reliable) performance metrics?

## Usage

``` r
Converge(
  MSEobj,
  PMs = c("Yield", "P10", "AAVY"),
  maxMP = 15,
  thresh = 0.5,
  ref.it = 20,
  inc.leg = FALSE,
  all.its = FALSE,
  nrow = NULL,
  ncol = NULL,
  silent = FALSE
)
```

## Arguments

- MSEobj:

  An MSE object of class `'MSE'`

- PMs:

  A character vector of names of the PM methods or a list of the PM
  methods

- maxMP:

  Maximum number of MPs to include in a single plot

- thresh:

  The convergence threshold. Maximum root mean square deviation over the
  last `ref.it` iterations

- ref.it:

  The number of iterations to calculate the convergence statistics. For
  example, a value of 20 means convergence diagnostics are calculated
  over last 20 simulations

- inc.leg:

  Logical. Should the legend be displayed?

- all.its:

  Logical. Plot all iterations? Otherwise only (nsim-ref.it):nsim

- nrow:

  Numeric. Optional. Number of rows

- ncol:

  Numeric. Optional. Number of columns

- silent:

  Hide the messages printed in console?

## Value

A table of convergence results for each MP

## Details

Performance metrics are plotted against the number of simulations.
Convergence diagnostics are calculated over the last `ref.it` (default =
20) iterations. The convergence diagnostics are:

1.  Is the order of the MPs stable over the last `ref.it` iterations?

2.  Is the average difference in performance statistic over the last
    `ref.it` iterations \< `thresh`?

By default three commonly used performance metrics are used:

1.  Average Yield Relative to Reference Yield

2.  Probability Spawning Biomass is above 0.1BMSY

3.  Probability Average Annual Variability in Yield is \< 20 per cent

Additional or alternative performance metrics objects can be supplied.
Advanced users can develop their own performance metrics.

## Author

A. Hordyk

## Examples

``` r
if (FALSE) { # \dontrun{
MSE <- runMSE()
Converge(MSE)
} # }
```
