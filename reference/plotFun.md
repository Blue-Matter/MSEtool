# Print out plotting functions

This function prints out the available plotting functions for objects of
class MSE or Data

## Usage

``` r
plotFun(class = c("MSE", "Data"), msg = TRUE)
```

## Arguments

- class:

  Character string. Prints out the plotting functions for objects of
  this class.

- msg:

  Logical. Should the functions be printed to screen?

## Note

Basically the function looks for any functions in the MSEtool that have
the word `plot` in them. There is a chance that some plotting functions
are missed. Let us know if you find any and we will add them.

## Author

A. Hordyk
