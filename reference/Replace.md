# Replace an existing Stock, Fleet, Obs, or Imp object

A function that replaces a Stock, Fleet, Obs, or Imp object from an OM
with one from another object.

## Usage

``` r
Replace(
  OM,
  from,
  Sub = c("Stock", "Fleet", "Obs", "Imp"),
  Name = NULL,
  silent = FALSE
)
```

## Arguments

- OM:

  An operating model object (class OM) which will be updated with a
  sub-model from another OM

- from:

  An object of class `OM`, `Stock`, `Fleet`, `Obs`, or `Imp` to be
  replace the values in `OM`

- Sub:

  A character string specifying what object type to replace (only used
  if `from` is class `OM`) "Stock", "Fleet", "Obs", or "Imp" (default is
  all four which is probably not what you want to do)

- Name:

  Character. Name for the new OM object (`OM@Name`)

- silent:

  Should messages be printed?

## Value

An object of class OM

## Author

A. Hordyk

## Examples

``` r
# Replace Stock
OM <- MSEtool::testOM
OM2 <- Replace(OM, Blue_shark)
#> ✔ Replacing sub-model:  Stock

# Replace Fleet
OM <- MSEtool::testOM
OM2 <- Replace(OM, Generic_DecE)
#> ✔ Replacing sub-model:  Fleet

# Replace Fleet from another OM
# OM1 <- new("OM", Albacore, Generic_DecE, Perfect_Info, Overages)
# OM2 <- new("OM", Blue_shark, Generic_IncE, Generic_Obs, Perfect_Imp)
# OM1a <- Replace(OM1, OM2, "Fleet")
```
