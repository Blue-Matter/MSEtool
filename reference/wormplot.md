# Biomass wormplot

A worm plot for plotting the likelihood of meeting biomass targets in
future years.

## Usage

``` r
wormplot(MSEobj, Bref = 0.5, LB = 0.25, UB = 0.75)
```

## Arguments

- MSEobj:

  Object of class MSE, output of the runMSE function

- Bref:

  The reference fraction of BMSY (to evaluate the probability of
  exceeding this level)

- LB:

  The lower bound probability that separates red (bad) and yellow (O.K.)
  colored segments

- UB:

  The upper bound probability that separates yellow (O.K.) and green
  (good) colored segments

## Details

Returns a matrix of nMPs rows and proyears columns which is the fraction
of simulations for which biomass was above Bref.

## Author

T. Carruthers
