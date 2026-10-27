# Plot several plots with a shared legend

Plot several plots with a shared legend

## Usage

``` r
join_plots(
  plots,
  ncol = length(plots),
  nrow = 1,
  position = c("right", "bottom"),
  legend = TRUE
)
```

## Arguments

- plots:

  list of plot objects of class `gg` or `ggplot`

- ncol:

  Optional number of columns

- nrow:

  Optional number of rows

- position:

  position of the legend ("bottom" or "right")

- legend:

  Logical. Use a legend?

## Note

modified from
https://github.com/tidyverse/ggplot2/wiki/share-a-legend-between-two-ggplot2-graphs
