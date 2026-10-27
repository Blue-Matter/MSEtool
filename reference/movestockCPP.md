# Apply the movement model to the stock for one time-step

Apply the movement model to the stock for one time-step

## Usage

``` r
movestockCPP(nareas, maxage, mov, Number)
```

## Arguments

- nareas:

  The number of spatial areas

- maxage:

  The maximum age

- mov:

  Numeric matrix (nareas by nareas) with the movement matrix

- Number:

  A numeric matrix (maxage+1, nareas) with current numbers-at-age in
  each area

## Author

A. Hordyk
