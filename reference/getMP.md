# Search R session for MP

Calls [`dynGet()`](https://rdrr.io/r/base/get.html), then
[`get()`](https://rdrr.io/r/base/get.html) in order to find the MP
definition in the R session.

## Usage

``` r
getMP(MP)
```

## Arguments

- MP:

  Character of MP name

## Value

The function definition or an error message from
[`try()`](https://rdrr.io/r/base/try.html) if unsuccessful

## Author

Q. Huynh
