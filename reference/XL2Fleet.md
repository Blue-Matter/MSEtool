# Import Fleet Object from Excel file

Imports a Fleet Object from a correctly formatted Excel file.

## Usage

``` r
XL2Fleet(name = NULL, cpars = NULL, msg = TRUE)
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

An object of class Fleet

## Details

An error message will alert if any slots are missing values, or if the
Excel file is missing the required tabs.

## Author

A. Hordyk
