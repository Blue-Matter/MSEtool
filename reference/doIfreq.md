# Create indices that are sampled at various frequencies

Given an index (historical period and projected period) this function
creates sparsity in the projected index to simulate varying frequency
(intensity) of data collection.

## Usage

``` r
doIfreq(I_hist, I_freq, LHYr, CurYr, Year)
```

## Arguments

- I_hist:

  Vector of real numbers, concatinated observed (historical) and
  simulated (projected) indices.

- I_freq:

  Positive integer. The frequency of index sampling (e.g. 1 is every
  year, 2 is every 2 years - a gap every 2 years in the projected,
  simulated data).

- LHYr:

  Positive integer, a year (e.g. 2023), the last historical year,
  demarks the historical period where observations have been collected
  from the projected period where sparsity is to be simulated.

- CurYr:

  Positive integer, a year (e.g. 2043), the most recent year of the
  simulation.

- Year:

  Vector of positive integers (as long as I_hist), the years
  corresponding with I_hist.

## Value

A thinned ector I_hist long of index observations.

## Author

T. Carruthers
