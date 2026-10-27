# Reads data Stock Synthesis file structure into a Data object using package r4ss

A function that uses the file location of a fitted SS3 model including
input files to population the various slots of an Data object.

## Usage

``` r
SS2Data(
  SSdir,
  Name = "Imported by SS2Data",
  Common_Name = "",
  Species = "",
  Region = "",
  min_age_M = 1,
  gender = 1,
  comp_fleet = "all",
  comp_season = "sum",
  comp_partition = "all",
  comp_gender = "all",
  index_season = "mean",
  silent = FALSE,
  ...
)
```

## Arguments

- SSdir:

  A folder with Stock Synthesis input and output files in it

- Name:

  The name for the Data object

- Common_Name:

  Character string for the common name of the stock.

- Species:

  Scientific name of the species

- Region:

  Geographic region of the stock or fishery.

- min_age_M:

  Currently, the Data object supports a single value of M for all ages.
  The argument selects the minimum age for calculating the mean of
  age-dependent M from the SS assessment.

- gender:

  An integer index for the sex for importing biological parameters (1 =
  female, 2 = male).

- comp_fleet:

  A vector of indices corresponding to fleets in the assessment over
  which to aggregate the composition (catch-at-length and catch-at-age)
  data. By default, character string `"all"` will aggregate across all
  fleets.

- comp_season:

  Integer, for seasonal models, the season for which the value of the
  index will be used. By default, `"mean"` will take the average across
  seasons.

- comp_partition:

  Integer vector for selecting length/age observations that are retained
  (2), discarded (1), or both (0). By default, `"all"` sums over all
  available partitions.

- comp_gender:

  Integer vector for selecting length/age observations that are female
  (1), male (2), or both (0), or both scaled to sum to one (3). By
  default, `"all"` sums over all gender codes.

- index_season:

  Integer, for seasonal models, the season for which the value of the
  index will be used. By default, `"mean"` will take the average across
  seasons.

- silent:

  Logical. Suppress all messages?

- ...:

  Arguments to pass to `r4ss::SS_output()`

## Value

An object of class Data.

## Note

Currently supports the version of r4ss on CRAN (v.1.24) and Github
(v.1.34-40). Function may be incompatible with other versions of r4ss.

## See also

[SS2OM](https://msetool.openmse.com/reference/SS2MOM.md)

## Author

T. Carruthers and Q. Huynh
