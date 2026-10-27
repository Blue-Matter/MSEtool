# Depletion and F estimation from mean length of catches

A highly dubious means of getting very uncertain estimates of current
stock biomass and (equilibrium) fishing mortality rate from growth,
natural mortality rate, recruitment and fishing selectivity.

## Usage

``` r
ML2D(OM, ML, nsim = 100, ploty = T, Dlim = c(0.05, 0.6))
```

## Arguments

- OM:

  An object of class 'OM'

- ML:

  A estimate of current mean length of catches

- nsim:

  Number of simulations

- ploty:

  Produce a plot of depletion and F

- Dlim:

  Limits on the depletion that is returned as a fraction of unfished
  biomass.

## Value

An object of class 'OM' with 'D' slot populated

## Author

T. Carruthers
