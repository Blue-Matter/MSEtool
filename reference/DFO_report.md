# Create a standard DFO MSE report

Provides performance plots typical in the assessment of Canadian fish
stocks.

## Usage

``` r
DFO_report(
  MSEobj,
  output_file = NA,
  author = "Author not specified",
  title = NA,
  maxMPs = 15
)
```

## Arguments

- MSEobj:

  An object of class MSE

- output_file:

  The directory and filename you wish to use for the report e.g.
  "C:/temp/myMSEreport.html"

- author:

  The person who made this report

- title:

  The title of the report

- maxMPs:

  Maximum number of MPs to plot

## Author

T. Carruthers
