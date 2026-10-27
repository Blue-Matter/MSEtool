# Reads data Stock Synthesis file structure into a nested Data object analogous with multiMSE

A function that uses the file location of a fitted SS3 model including
input files to population the various slots of an Data object.

## Usage

``` r
SS2DataMOM(SSdir, age_M = NULL, comp_partition = 2, silent = FALSE, ...)
```

## Arguments

- SSdir:

  A folder with Stock Synthesis input and output files in it.
  Alternatively,

- age_M:

  A vector of ages to average across to calculate a single value of
  natural mortality. Currently, the Data object supports a single value
  of M for all ages. By default, `NULL` averages over all ages.

- comp_partition:

  Integer vector for selecting length/age observations that are retained
  (2), discarded (1), or both (0). By default, only retained comps are
  used. If multiple codes are used, then comp matrix is the sum over all
  codes.

- silent:

  Logical. Suppress messages?

- ...:

  Arguments to pass to `r4ss::SS_output()`

## Value

A nested list of Data objects, with the first index by stock/sex and the
second index by fleet.

## Note

Currently tested on r4ss version 1.38.1-41 and SS 3.30.14.

Catches in `Data@Cat` are the predicted sex-specific catch calculated
from the SS output.

## See also

[SS2MOM](https://msetool.openmse.com/reference/SS2MOM.md)

## Author

Q. Huynh
