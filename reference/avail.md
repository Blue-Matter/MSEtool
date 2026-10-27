# What objects of this class are available

Generic class finder

## Usage

``` r
avail(classy, package = NULL, msg = TRUE, abc = FALSE)
```

## Arguments

- classy:

  A class of object (character string, e.g. 'Fleet')

- package:

  Optional. Names(s) of the package to search for object of class
  `classy`. String Default is all `openMSE` packages. Always searches
  the global environment as well.

- msg:

  Print messages?

- abc:

  Logical, whether to alphabetize the results. By default, the function
  returns results found in the global environment, then core openMSE
  packages, and any additional packages in argument `package`.

## Details

Finds objects of the specified class in the global environment or the
openMSE packages.

## See also

[Can](https://msetool.openmse.com/reference/Can.md)
[Cant](https://msetool.openmse.com/reference/Can.md) avail

## Author

T. Carruthers

## Examples

``` r
avail("OM", msg=FALSE)
#> [1] "testOM"
Stocks <- avail("Stock")
#> → Searching for objects of class "Stock" in package "MSEtool"
Fleets <- avail("Fleet")
#> → Searching for objects of class "Fleet" in package "MSEtool"
MPs <- avail("MP")
#> → Searching for objects of class "MP" in package "MSEtool"
```
