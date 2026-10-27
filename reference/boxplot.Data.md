# Boxplot of TAC recommendations

Boxplot of TAC recommendations

## Usage

``` r
# S3 method for class 'Data'
boxplot(x, upq = 0.9, lwq = 0.1, ylim = NULL, outline = FALSE, col = NULL, ...)
```

## Arguments

- x:

  An object of class MSE

- upq:

  Upper quantile of TACs for max ylim

- lwq:

  Lower quantile of TACs for min ylim

- ylim:

  Optional numeric vector of length 2 to specify limits of y-axis.

- outline:

  Logical. Include outliers in plot?

- col:

  Optional colours to pass to `boxplot`

- ...:

  Optional additional arguments passed to `boxplot`

## Value

Returns a data frame containing the information shown in the plot

## Author

A. Hordyk
