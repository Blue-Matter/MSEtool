# MP feasibility diagnostic using real data

What MPs do not return NAs from the real data

## Usage

``` r
RealFease(Data = NULL)
```

## Arguments

- Data:

  An object of class 'Data'. Optional. If Data object is included, the
  returned MPs are both feasible (in terms of management) and possible
  (sufficient data to run MP)

## Value

a vector of MP names that calculate without errors for the specific
data.

## Author

T. Carruthers
