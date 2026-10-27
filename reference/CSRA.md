# Catch at size reduction analysis

What depletion level and corresponding equilibrium F arise from data
regarding mean length of current catches, natural mortality rate,
steepness of the stock recruitment curve, maximum length, maximum growth
rate, age at maturity, age based vulnerability, maturity at age, maximum
age and number of historical years of fishing.

## Usage

``` r
CSRA(M, h, Linf, K, t0, AM, a, b, vuln, mat, ML, CAL, CAA, maxage, nyears)
```

## Arguments

- M:

  A vector of natural mortality rate estimates

- h:

  A vector of sampled steepness (Beverton-Holt stock recruitment)

- Linf:

  A vector of maximum length (von Bertalanffy growth)

- K:

  A vector of maximum growth rate (von Bertalanffy growth)

- t0:

  A vector of theoretical age at length zero (von Bertalanffy growth)

- AM:

  A vector of age at maturity

- a:

  Length-weight conversion parameter a (W=aL^b)

- b:

  Length-weight conversion parameter b (W=aL^b)

- vuln:

  A matrix nsim x nage of the vulnerability at age (max 1) to fishing.

- mat:

  A matrix nsim x nage of the maturity at age (max 1)

- ML:

  A vector of current mean length estimates

- CAL:

  A catch-at-length matrix nyears x (1 Linf unit) length bins

- CAA:

  A catch-at-age matrix nyears x maximum age

- maxage:

  Maximum age

- nyears:

  Number of historical years of fishing

## Author

T. Carruthers
