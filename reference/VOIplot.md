# Yet another Value of Information Plot

A function that relates parameters of the observation model and the
operating model parameters to yield.

## Usage

``` r
VOIplot(
  MSEobj,
  MPs = NA,
  nvars = 5,
  nMP = 4,
  Par = c("Obs", "OM"),
  YVar = c("Y", "B"),
  doPlot = TRUE,
  incStat = FALSE,
  availMP = NULL,
  acceptMP = NULL,
  incNames = TRUE,
  labcex = 0.8,
  quants = c(0.05, 0.95)
)
```

## Arguments

- MSEobj:

  An object of class MSE

- MPs:

  The MPs to plot. If NA it will plot the first nMP from MSEobj

- nvars:

  The number of observation or operating model parameters to plot
  (number of columns)

- nMP:

  The maximum number of MPs to plot (number of rows)

- Par:

  Plot Operating Model (OM) or Observation (Obs) parameters?

- YVar:

  Variable for Y-Axis: Yield (Y) or Biomass (B) (relative to BMSY)

- doPlot:

  Output the plot?

- incStat:

  Include a print out of statistic describing the curviness of the line?

- availMP:

  Optional character string of MPs that are available. These names are
  colored black

- acceptMP:

  Optional character string of MPs that are acceptable. These names are
  colored green if they are also in availMP

- incNames:

  Include the names?

- labcex:

  Character size of the label

- quants:

  Quantiles to calculate

## Value

A list of all the information included in the plot

## Author

A. Hordyk
