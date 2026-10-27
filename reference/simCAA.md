# Simulate Catch-at-Age Data

CAA generated with either a multinomial or logistic normal observation
model from retained catch-at-age array

## Usage

``` r
simCAA(nsim, yrs, n_age, Cret, CAA_ESS, CAA_nsamp)
```

## Arguments

- nsim:

  Number of simulations

- yrs:

  Number of years

- n_age:

  Number of age classes

- Cret:

  Retained Catch at age in numbers - array(sim, years, maxage+1)

- CAA_ESS:

  CAA effective sample size. If greater than 1, then this is the
  multinomial distribution sample size. If less than 1, this is the
  coefficient of variation for the logistic normal distribution (see
  details).

- CAA_nsamp:

  CAA sample size

## Value

CAA array

## Details

The logistic normal generates the catch-at-age sample by first sampling
once from a multivariate normal distribution with the mean vector equal
to the logarithm of the proportions-at-age and the diagonal of the
covariance matrix is the square of the product of the CV and the log
proportions (all off-diagonals are zero). The sampled vector is then
converted to proportions with the softmax function and expanded to
numbers (CAA_nsamp). This method allows for simulating fractional values
in the catch-at-age matrix.
