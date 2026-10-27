# Manually map parameters for the historical period of operating model

Interactive plots to specify trends and variability in fishing effort,
fleet selectivity, and natural mortality for the operating model.

## Usage

``` r
ChooseEffort(Fleet, Years = NULL)

ChooseM(OM, type = c("age", "length"), x = NULL, y = NULL)

ChooseSelect(Fleet, Stock, FstYr = NULL, SelYears = NULL)
```

## Arguments

- Fleet:

  A fleet object.

- Years:

  An optional vector of years. Should be nyears long.

- OM:

  An object of class 'OM'

- type:

  A character string - is M to be mapped by 'age' or 'length'?

- x:

  Optional vector for x-axis

- y:

  Optional vector for y-axis

- Stock:

  Optional Stock object. If provided, average length-at-maturity is
  included on plot for reference.

- FstYr:

  Optional value for first historical year. If empty, user must specify
  the year in console.

- SelYears:

  Optional vector of values for each year where selectivity pattern
  changed. If empty, user must specify the years in console (comma
  separated).

## Value

`ChooseEffort` and `ChooseSelect` return a Fleet object while `ChooseM`
returns an OM object.

## Details

|  |  |
|----|----|
| `ChooseEffort` | Interactive plot which allows users to specify the relative trajectory and variability in the historical fishing effort and populates Fleet object. |
| `ChooseM` | Interactive plot which allows users to specify M by age or size class |
| `ChooseSelect` | Input the first historical year, and all years where selectivity pattern changed (separated by comma). Interactive plot which allows users to specify a range for the length at 5\\ selectivity at maximum length for each year. Produces a simple plot which shows the range in selectivity pattern for each break-point year. Selectivity-at-length is fixed in between break-point years. Note that this function replaces 'nyears' in the Fleet object with the value defined here (FstYr:current year). |

## Author

A. Hordyk
