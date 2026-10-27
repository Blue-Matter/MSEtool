# Import a Data object from Excel file

Import a Data object from Excel file

## Usage

``` r
XL2Data(name, dec = c(".", ","), sheet = 1, silent = FALSE)
```

## Arguments

- name:

  Name of the data file, with or without file extension. Include full
  file path if not in working directory

- dec:

  the character used in the file for decimal points.

- sheet:

  Sheet number if importing Data from XL file

- silent:

  Logical. Hide messages?

## Value

An object of class 'Data'

## Author

A. Hordyk

## Examples

``` r
if (FALSE) { # \dontrun{
MyData <- XL2Data("MyData.xlsx")
} # }
```
