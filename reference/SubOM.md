# Subset a Stock, Fleet, Obs, or Imp object from an OM object

A function that strips out a Stock, Fleet, Obs, or Imp object from a
complete OM object. Mainly used for internal functions.

## Usage

``` r
SubOM(OM, Sub = c("Stock", "Fleet", "Obs", "Imp"))
```

## Arguments

- OM:

  An operating model object (class OM)

- Sub:

  A character string specifying what object type to strip out "Stock",
  "Fleet", "Obs", or "Imp"

## Value

An object of class Stock, Fleet, Obs, or Imp

## See also

[Sub](https://msetool.openmse.com/reference/Sub.md) for subsetting MSE
output and [SubCpars](https://msetool.openmse.com/reference/SubCpars.md)
for subsetting by simulation and projection years.

## Author

A. Hordyk

## Examples

``` r
Stock <- SubOM(testOM, "Stock")
class(Stock)
#> [1] "Stock"
#> attr(,"package")
#> [1] "MSEtool"
```
