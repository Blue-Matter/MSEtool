# Length to age conversion

Simple deterministic length to age conversion given inverse von
Bertalanffy growth.

## Usage

``` r
L2A(t0c, Linfc, Kc, Len, maxage, ploty = F)
```

## Arguments

- t0c:

  Theoretical age at length zero

- Linfc:

  Maximum length

- Kc:

  Maximum growth rate

- Len:

  Length

- maxage:

  Maximum age

- ploty:

  Should a plot be included

## Value

An age (vector of ages, matrix of ages) corresponding with Len

## Author

T. Carruthers
