# Calculates movement matrices from user inputs for fraction in each area (fracs) and probability of staying in areas (prob)

A function for calculating a movement matrix from user specified
unfished stock biomass fraction in each area. Used by
[simmov](https://msetool.openmse.com/reference/simmov.md) to generate
movement matrices for an operating model.

## Usage

``` r
makemov(fracs = c(0.1, 0.2, 0.3, 0.4), prob = c(0.5, 0.8, 0.9, 0.95))
```

## Arguments

- fracs:

  A vector nareas long of fractions of unfished stock biomass in each
  area

- prob:

  A vector of the probability of individuals staying in each area or a
  single value for the mean probability of staying among all areas

## See also

[simmov](https://msetool.openmse.com/reference/simmov.md)

## Author

T. Carruthers
