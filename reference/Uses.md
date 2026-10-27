# Find the Management Procedures that use a particular data slot

Find the Management Procedures that use a particular data slot

## Usage

``` r
Uses(slot, silent = FALSE)
```

## Arguments

- slot:

  A slot from an object of class `Data`. Character string.

- silent:

  Logical. Should messages be printed?

## Value

A character string of MPs that use the slot.

## Author

A. Hordyk

## Examples

``` r
Uses("Mort")
#> ✔ No MPs used Data slot ' Mort '.
#> NULL
```
