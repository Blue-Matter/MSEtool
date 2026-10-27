# Directory of the data in the installed package on your computer

A way of locating where the package was installed so you can find
example data files and code etc.

## Usage

``` r
DataDir(stock = NA)
```

## Arguments

- stock:

  Character string representing the name of a .csv file e.g. 'Snapper',
  'Rockfish'

## Value

The file path to the object

## Author

T. Carruthers

## Examples

``` r
if (FALSE) { # \dontrun{
tilefish_location <- DataDir("Gulf_blue_tilefish")
tilefish_Data <- new("Data", tilefish_location)
} # }
```
