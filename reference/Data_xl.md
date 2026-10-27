# Read in Data object from Excel spreadsheet

A function to read in Data object from an Excel spreadsheet with tabs
named following specific convention.

## Usage

``` r
Data_xl(fname, stkname, fpath = "", saveCSV = FALSE)
```

## Arguments

- fname:

  Name of the Excel spreadsheet file. Must include file extension.

- stkname:

  Name of the Stock.

- fpath:

  Full file path, if file is not in current working directory

- saveCSV:

  Do you also want to the Data parameters to a CSV file?

## Value

A object of class Data

## Details

The Excel spreadsheet must have tabs named with the following
convention. For example if `stkname` is 'myFish', the Data parameters
are in a tab named 'myFishData'.

## Author

A. Hordyk

## Examples

``` r

if (FALSE) { # \dontrun{
OM <- OM_xl(fname='OMTables.xlsx', stkname='myFish')
} # }
```
