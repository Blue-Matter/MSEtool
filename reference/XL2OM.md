# Load OM from Excel file

Imports an OM from a correctly formatted Excel file. Create the Excel
spreadsheet template using `OMinit` and document each slot in the
corresponding text file.

## Usage

``` r
XL2OM(name = NULL, cpars = NULL, msg = TRUE)
```

## Arguments

- name:

  Name of the OM Excel file. Provide full file path if not in current
  directory.

- cpars:

  An optional list of custom parameters (single parameters are a vector
  nsim long, time series are a matrix nsim rows by nyears columns)

- msg:

  Should messages be printed?

## Value

An object of class OM

## Details

An error message will alert if any slots are missing values, or if the
Excel file is missing the required tabs.

## Author

A. Hordyk

## Examples

``` r
if (FALSE) { # \dontrun{
OMinit('myOM', templates=list(Stock='Herring', Fleet='Generic_Fleet', Obs='Generic_Obs',
Imp='Perfect_Imp'), overwrite=TRUE)
myOM <- XL2OM('myOM.xlsx')

} # }
```
