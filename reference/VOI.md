# Calculate Value Of Information

A function that relates operating model parameters and parameters of the
observation model to yield (by default). A user can also specific their
own utility values (Ut) which is arranged in a matrix of nsim rows and
nMP columns.

## Usage

``` r
VOI(
  MSEobj,
  ncomp = 6,
  nbins = 8,
  maxrow = 8,
  Ut = NA,
  Utnam = "Utility",
  plot = TRUE
)
```

## Arguments

- MSEobj:

  An object of class MSE

- ncomp:

  Maximum number of variables to examine per MP

- nbins:

  Number of percentile bins for sampled parameters of the operating
  model or observation model, which is used for calculating variability
  in utility across the sampled range of each parameter

- maxrow:

  maximum number of MPs per plot

- Ut:

  A matrix of user-specified utility values of nsim rows and nMPs
  columns

- Utnam:

  The name of the utility measure for plotting

- plot:

  Logical. Show the plot?

## Author

T. Carruthers
