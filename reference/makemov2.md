# Calculates movement matrices from user inputs for fraction in each area (fracs) the relative fraction moving to other areas, plus a mean probability of staying in any given area.

A function for calculating a movement matrix from user specified
distribution among areas (v) and relative movement to other areas
(solves for positive diagonal - vector of prob staying). Used by
[simmov2](https://msetool.openmse.com/reference/simmov2.md) to generate
movement matrices for an operating model. There must be a prior on the
positive diagonal of the movement matrix or these will tend to 1 and
hence perfectly satisfy the requirement V = MV.

## Usage

``` r
makemov2(
  dist = c(0.05, 0.6, 0.35),
  prob = 0.5,
  probE = 1,
  frac_other = matrix(c(NA, 2, 1, 2, NA, 1, 1, 2, NA), nrow = 3, byrow = T),
  plot = F
)
```

## Arguments

- dist:

  A vector nareas long of fractions of unfished stock biomass in each
  area

- prob:

  A vector of the probability of individuals staying in each area or a
  single value for the mean probability of staying among all areas

- probE:

  The logit CV associated with prob (used as a penalty when optimizing
  for diagonal)

- frac_other:

  A matrix nareas x nareas that specifies the relative fraction moving
  from one area to the others. The positive diagonal is unspecified.

- plot:

  Should the convergence to a stable distribution be plotted?

## See also

[simmov2](https://msetool.openmse.com/reference/simmov2.md)

## Author

T. Carruthers
