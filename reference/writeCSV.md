# Internal function to write CSVs for objects

Used internally in the DLMtool package to write CSV files from an
existing DLMtool object

## Usage

``` r
writeCSV(
  inobj,
  tmpfile = NULL,
  objtype = c("Stock", "Fleet", "Obs", "Imp", "Data", "OM")
)
```

## Arguments

- inobj:

  A object of class Stock, Fleet, Obs, Imp, Data, or OM

- tmpfile:

  The full file path and name for the saved CSV file

- objtype:

  The class corresonding to the `inobj`

## Author

A. Hordyk
