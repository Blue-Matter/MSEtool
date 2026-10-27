# Combines indices into a single index using linear modelling (\*\* Deprecated \*\*)

iSCAM assessments often make use of multiple indices of abundance. The
data object and MPs currently only make use of a single index.
combiSCAMinds is a function that creates a single index from many using
linear modelling. It is a simple way of providing initial calculations
of management recommendations and it should be noted that this process
is important and in a real application would require due diligence (ie
peer reviewed data workshop).

## Usage

``` r
iSCAMinds(idata, Year, fleeteffect = T)
```

## Arguments

- idata:

  List: the indices recorded in a read from an iSCAM data folder, e.g.
  replist\$data\$indices

- Year:

  Integer vector: the years of the data object ie Data@Year

- fleeteffect:

  Logical: should a fleet effect be added to the linear model?

## Author

T. Carruthers
