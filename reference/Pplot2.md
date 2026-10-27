# A projection by projection plot of F/FMSY, B/BMSY, B/B0, and yield

A projection by projection plot of F/FMSY, B/BMSY, B/B0, and yield

## Usage

``` r
Pplot2(
  MSEobj,
  YVar = c("F_FMSY", "SSB_SSBMSY"),
  MPs = NA,
  sims = NULL,
  traj = c("all", "quant", "both"),
  quants = c(0.1, 0.9),
  incquant = TRUE,
  quantcol = "lightgray",
  RefYield = c("lto", "curr"),
  LastYr = TRUE,
  ref.lines = c(0.5, 1, 1.5),
  maxMP = 6,
  alpha = 60,
  cex.axis = 1,
  cex.lab = 1,
  YLab = NULL,
  incMP = TRUE,
  MPcex = 1,
  MPcol = "black",
  incLeg = TRUE,
  cex.leg = 1.5,
  legPos = "topleft",
  yline = NULL,
  xline = NULL,
  parOR = FALSE,
  xaxis = TRUE,
  yaxis = TRUE,
  oneIt = TRUE,
  ...
)
```

## Arguments

- MSEobj:

  An object of class MSE

- YVar:

  What to plot on the y-axis? Options are:
  `c('SSB_SSB0', 'SSB_SSBMSY', 'F_FMSY', 'Yield')`

- MPs:

  Optional subset by MP

- sims:

  Optional subset by simulation

- traj:

  Plot all projections (`all`), only quantiles (`quant`), or both
  projections and median (`both`)

- quants:

  Numeric vector of length 2 specifying the quantiles (e.g., 10th and
  90th. Median is always included)

- incquant:

  Logical. Include the quantiles or only plot median?

- quantcol:

  Colour of the quantile polygon

- RefYield:

  Should yield be relative to long-term optimum (`lto`) or last
  historical year (`curr`)

- LastYr:

  Logical. Include the last historical year in the yield projections?

- ref.lines:

  Numeric vector of y-values for horizontal reference lines. Set to NULL
  to remove lines.

- maxMP:

  Maximum number of MPs to plot

- alpha:

  Alpha for transparency of lines

- cex.axis:

  Size of axis text

- cex.lab:

  Size of axis label

- YLab:

  Optional label for y-axis

- incMP:

  Logical. Include name of MP?

- MPcex:

  Size of MP label

- MPcol:

  Optional character vector of colors for MP labels

- incLeg:

  Logical. Include a legend?

- cex.leg:

  Size of legend text

- legPos:

  Legend position

- yline:

  Optional horizontal lines

- xline:

  Optional vertical lines

- parOR:

  Logical to over-ride the par parameters

- xaxis:

  Logical. Should x-axis labels be displayed?

- yaxis:

  Logical. Should y-axis labels be displayed?

- oneIt:

  Logical. Should one iteration be plotted on the quantile plot?

- ...:

  Additional arguments to be passed to plotting functions

## Author

T. Carruthers & A.Hordyk
