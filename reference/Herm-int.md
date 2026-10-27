# Internal Herm functions

- `expandHerm` expands the Herm list in SexPars to a matrix of fractions
  at age

&nbsp;

- `checkHerm` checks that each array in the list has dimension nsim x
  maxage+1 x nyears + proyears. For backwards compatibility, also
  converts matrices to arrays by adding the year dimension.

&nbsp;

- `subsetHerm` returns year-specific Herm values.

## Usage

``` r
expandHerm(Herm, maxage, np, nsim)

checkHerm(Herm, maxage, nsim, nyears, proyears)

subsetHerm(Herm, y)
```

## Arguments

- Herm:

  A list of Hermaphroditic fractions at age

- maxage:

  The maximum age of stocks being simulated

- np:

  The total number of stocks being simulated

- nsim:

  The number of simulations

- nyears:

  The number of historical years

- proyears:

  The number of projection years

- y:

  The year to subset

## Author

T. Carruthers

Q. Huynh
