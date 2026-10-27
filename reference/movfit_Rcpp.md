# Rcpp version of the Optimization function that returns the squared difference between user specified and calculated movement parameters.

The user specifies the probability of staying in the same area and
spatial heterogeneity (both in the unfished state). This function
returns the squared difference between these values and those produced
by the three logit movement model.

## Usage

``` r
movfit_Rcpp(par, prb, frac)
```

## Arguments

- par:

  Three parameters in the logit space that control the four
  probabilities of moving between 2 areas

- prb:

  User specified probability that individuals in area 1 remain in that
  area (unfished conditions)

- frac:

  User specified fraction of individuals found in area 1 (unfished
  conditions)

## Details

This is paired with getmov to find the correct movement model.

## Author

T. Carruthers with an amateur attempt at converting to Rcpp by A. Hordyk
(but it works!)
