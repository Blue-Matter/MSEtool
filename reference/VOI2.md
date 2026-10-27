# Calculate Value Of Information 2

A function that relates operating model parameters and parameters of the
observation model to relative yield (yield over last 5 years of
projection relative to a 'best F' scenario that maximizes yield).

## Usage

``` r
VOI2(MSEobj, ncomp = 6, nbins = 4, Ut = NA, Utnam = "yield", lay = F)
```

## Arguments

- MSEobj:

  An object of class MSE

- ncomp:

  Maximum number of observation variables to examine per MP

- nbins:

  Number of bins for sampled observation variables used for calculating
  variability in utility across the sampled range of each parameter

- Ut:

  A matrix of user-specified utility values of nsim rows and nMPs
  columns

- Utnam:

  The name of the utility measure for plotting

- lay:

  Controls whether labels are in lay terms or not

## Note

VOI2 assumes that relative cost for each type of improvement in data is
linearly related to the number of samples (e.g. nCAAobs) or square
function of improved precision and bias e.g.: relative cost=
1/(newCV/oldCV)^2

## Author

T. Carruthers
