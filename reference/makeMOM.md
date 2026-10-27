# Utility for making multi-OMs

Converts an OM to a single stock, single fleet MOM.

## Usage

``` r
makeMOM(..., silent = FALSE)
```

## Arguments

- ...:

  An [OM](https://msetool.openmse.com/reference/OM-class.md).

- silent:

  Should messages be printed out to the console?

## Value

A class [MOM](https://msetool.openmse.com/reference/MOM-class.md)
object.

## Author

Q. Huynh

## Examples

``` r
MOM <- makeMOM(testOM)
```
