# KOBE plot: a projection by projection plot of F/FMSY and B/BMSY

A standard KOBE plot by each method that also shows the percentage of
methods that ended up in each quadrant.

## Usage

``` r
Kplot(
  MSEobj,
  maxsim = 60,
  MPs = NA,
  sims = NULL,
  maxMP = 9,
  nam = NA,
  cex.leg = 1.5
)
```

## Arguments

- MSEobj:

  An object of class MSE

- maxsim:

  Maximum number of simulations (lines) to plot on each panel.

- MPs:

  Optional subset MSE object by MP

- sims:

  Optional subset MSE object by simulation

- maxMP:

  Maximum number of MPs to include in plot

- nam:

  The name of the plot

- cex.leg:

  Size of legend

## Note

Apologies for the nauseating shading.

## Author

T. Carruthers with some additions from A. Hordyk
