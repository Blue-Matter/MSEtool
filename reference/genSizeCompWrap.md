# Wrapper for C++ function to generate length composition

And other internal related functions

## Usage

``` r
genSizeCompWrap(
  i,
  vn,
  CAL_binsmid,
  CAL_bins,
  retL,
  CAL_ESS,
  CAL_nsamp,
  Linfarray,
  Karray,
  t0array,
  LenCV,
  truncSD = 2
)

getfifth(lenvec, CAL_binsmid)
```

## Arguments

- i:

  Simulation number

- vn:

  Array of vulnerable numbers

- CAL_binsmid:

  Mid-points of CAL bins

- CAL_bins:

  Boundary of length bins

- retL:

  Array of retention-at-length

- CAL_ESS:

  CAL effective sample size

- CAL_nsamp:

  CAL sample size

- Linfarray:

  Matrix of Linf

- Karray:

  Matrix of K values

- t0array:

  Matrix of t0 values

- LenCV:

  Vector of LenCV

- truncSD:

  Numeric. Number of standard deviations to truncate normal d
  distribution

- lenvec:

  Vector of lengths

## Value

Generated length composition from `genSizeComp`

## Functions

- `getfifth()`: Internal function to calculate fifth percentile of size
  composition
