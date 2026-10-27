# Generic Trade-Plot Function

Generic Trade-Plot Function

## Usage

``` r
TradePlot(
  MSEobj,
  ...,
  Lims = c(0.2, 0.2, 0.8, 0.8),
  Title = NULL,
  Labels = NULL,
  Satisficed = FALSE,
  Show = "both",
  point.size = 2,
  lab.size = 4,
  axis.title.size = 12,
  axis.text.size = 10,
  legend = TRUE,
  legend.title.size = 12,
  position = c("right", "bottom"),
  cols = NULL,
  fill = "gray80",
  alpha = 0.4,
  PMlist = NULL,
  Refs = NULL,
  Yrs = NULL
)

Tplot(MSEobj, Lims = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5), ...)

Tplot2(MSEobj, Lims = c(0.2, 0.2, 0.8, 0.8), ...)

Tplot3(MSEobj, Lims = c(0.5, 0.5, 0.8, 0.5), ...)

NOAA_plot2(MSEobj)
```

## Arguments

- MSEobj:

  An object of class `MSE`

- ...:

  Names of Performance Metrics (PMs), or other arguments to `TradePlot`.
  First PM is recycled if number of PMs is not even

- Lims:

  A numeric vector of acceptable risk/minimum probability thresholds.
  Recycled if not equal to number of PMs.

- Title:

  Optional title for each plot. Character vector of `length(PMs)`/2.
  Recycled.

- Labels:

  Optional named list specifying new labels for MPs. For example:
  `Labels = list(AvC="Average Catch", CC1="Constant Catch")`

- Satisficed:

  Logical. Show only the MPs that meet minimum acceptable thresholds
  (specified in `Lims`)

- Show:

  Character. Show the plots ('plots'), results table ('table'), 'both'
  (default), or invisibly return objects only ('none')

- point.size:

  Numeric. Size of the MP points

- lab.size:

  Numeric. Size of MP label. Set to NULL to remove MP labels.

- axis.title.size:

  Numeric. Size of axis titles

- axis.text.size:

  Numeric. Size of axis text

- legend:

  Logical. Include legend?

- legend.title.size:

  Numeric. Size of legend title text

- position:

  Character. Position of legend - 'right' or 'bottom'

- cols:

  Optional character vector of colors for the legend (MP Types) or if
  `cols` is a character vector of length `MSEobj@nMPs`, then the MP
  labels are colored (no color legend).

- fill:

  Character. Color of the fill

- alpha:

  Numeric. Transparency of fill

- PMlist:

  Optional list of PM names. Overrides any supplied in ... above

- Refs:

  An optional named list (matching the PM names) with numeric values to
  override the default `Ref` values. See examples.

- Yrs:

  An optional named list (matching the PM names) with numeric values to
  override the default `Yrs` values. See examples.

## Value

Invisibly returns a list with summary table of MP performance and the
ggplot objects for the plots

## Functions

- `Tplot()`: A trade-off plot showing probabilities that:

  - not overfishing (PNOF) against long-term yield is \> 50\\

  - spawning biomass is below BMSY (P100) against LTY

  - spawning biomass is below 0.5BMSY (P50) against LTY

  - spawning biomass is below 0.1BMSY (P10) against LTY

- `Tplot2()`: A trade-off plot showing probabilities that:

  - short-term yield is \> 50\\

  - spawning biomass is below 0.1BMSY (P10) against average annual
    variability in yield is \< 20\\

- `Tplot3()`: A trade-off plot showing probabilities that:

  - not overfishing (PNOF) against long-term yield is \> 50\\

  - spawning biomass is below 0.1BMSY (P10) against average annual
    variability in yield is \< 20\\

- `NOAA_plot2()`: A trade-off plot developed for NOAA showing
  probabilities that:

  - not overfishing (PNOF) against long-term yield is \> 50\\

  - spawning biomass is below 0.5BMSY (P50) against average annual
    variability in yield is \< 15\\

## Author

A. Hordyk
