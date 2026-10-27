# An object for storing data for analysis using data-limited methods

Used internally

## Slots

- `Name`:

  Name of the Performance Metric. Character

- `Caption`:

  A caption to be used in plots. Character, call, or function.

- `Stat`:

  Statistic of interest for the PM. Dimensions: nsim, nMP, yrs. Array

- `Ref`:

  Reference value to calculate probability for statistic. Numeric.

- `Prob`:

  Probability (mean over years) Dimensions: nsim by MP. Matrix, numeric
  or data.frame

- `Mean`:

  Mean probability (mean over years and simulations). Numeric. Length
  nMPs

- `MPs`:

  Name of MPs. Single value. Character string

## Objects from the Class

Objects can be created by calls of the form `new('PMobj')`

## Author

A. Hordyk
