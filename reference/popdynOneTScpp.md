# Aging and Mortality for one time-step

Project population forward one time-step given current numbers-at-age
and total mortality

## Usage

``` r
popdynOneTScpp(nareas, maxage, Ncurr, Zcurr, plusgroup = 1L)
```

## Arguments

- nareas:

  The number of spatial areas

- maxage:

  The maximum age

- Ncurr:

  A numeric matrix (maxage+1, nareas) with current numbers-at-age in
  each area

- Zcurr:

  A numeric matrix (maxage+1, nareas) with total mortality-at-age in
  each area

## Author

A. Hordyk
