# Predict missing life-history parameters

Predict missing life-history based on taxonomic information and
hierarchical model fitted to FishBase life-history parameters

## Usage

``` r
LH2OM(
  OM,
  dist = c("unif", "norm"),
  filterMK = FALSE,
  plot = TRUE,
  Class = "predictive",
  Order = "predictive",
  Family = "predictive",
  msg = TRUE,
  db = MSEtool::LHdatabase
)

predictLH(
  inpars = list(),
  Genus = "predictive",
  Species = "predictive",
  nsamp = 100,
  db = MSEtool::LHdatabase,
  dist = c("unif", "norm"),
  filterMK = TRUE,
  plot = TRUE,
  Class = "predictive",
  Order = "predictive",
  Family = "predictive",
  msg = TRUE
)
```

## Source

<https://github.com/James-Thorson-NOAA/FishLife>

## Arguments

- OM:

  An object of class 'OM'

- dist:

  Character. Should parameters be sampled from a uniform (`unif`) or
  normal (`norm`) distribution?

- filterMK:

  Logical or numeric specifying percentiles. See Details. e.g. `OM@M`
  and `OM@K`. Empty slots or slots with all values of 0 are considered
  unknown.

- plot:

  Logical. Should the plot be produced?

- Class:

  Optional higher order taxonomic information

- Order:

  Optional higher order taxonomic information

- Family:

  Optional higher order taxonomic information

- msg:

  Logical. Should messages be printed?

- db:

  Database from FishLife model with fitted model results

- inpars:

  A named list with lower and upper bounds of provided parameters:
  *Linf*, *L50*, *K* and *M* (must be length 2). Unknown or missing
  parameters should not be included. For example, an empty list assumes
  that all four life history parameters are unknown and need to be
  estimated. See Details below for more information.

- Genus:

  Character string specifying the Genus name. Optional. Default is
  'predictive'

- Species:

  Character string specifying the Species name. Optional. Default is
  'predictive'. If full species name (Genus + Species) is not found if
  FishLife database (based on FishBase) higher order taxonomy will be
  used (e.g., Family) for the predictions.

- nsamp:

  The number of samples to return

## Value

LH2OM: An OM with `OM@cpars` populated with `OM@nsim` samples of M, K,
Linf and L50

predictLH: A data.frame with `nsamp` rows with `Linf`, `L50`, `K`, and
`M` values.

## Details

### filterMK

If filterMK is logical: Should the predicted M and K parameters be
filtered within the range specified in `inpars`or `OM`?

Otherwise, filterMK must be numeric vector of length(2) specifying lower
and upper percentiles that will be applied to the predicted M or K
values

The model predicts missing life-history parameters based on provided
parameters and taxonomic information. If both *M* and *K* are provided
in `inpars` or `OM`, *K* values are predicted and predictions filtered
so that resulting *K* values are within bounds specified in `inpars$K`
or `OM@K` (see `filterMK`).

If both *Linf* and *L50* are provided in `inpars` or `OM`, *L50* values
are predicted and values in `inpars$L50` or `OM@L50` are ignored.

## Functions

- `LH2OM()`: Predict missing life-history and populate `OM@cpars`

- `predictLH()`: Predict missing life-history based on taxonomic
  information and hierarchical model fitted to FishBase life-history
  parameters

## References

Thorson, J. T., S. B. Munch, J. M. Cope, and J. Gao. 2017. Predicting
life history parameters for all fishes worldwide. Ecological
Applications. 27(8): 2262–2276

## Author

A. Hordyk
