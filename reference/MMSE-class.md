# Class `'MMSE'`

A Multi Management Strategy Evaluation object that contains information
about simulation conditions and performance of MPs for a multi-stock,
multi-fleet operating model.

## Slots

- `Name`:

  Name of the MMSE object. Single value. Character string

- `nyears`:

  The number of years for the historical simulation. Single value.
  Positive integer

- `proyears`:

  The number of years for the projections - closed loop simulations.
  Single value. Positive integer

- `nMPs`:

  Number of management procedures simulation tested. Single value.
  Positive integer.

- `MPs`:

  The names of the MPs that were tested. Vector of length nMPs.
  Character strings.

- `MPcond`:

  The MP condition. Character ('bystock': an MP per stock, 'byfleet' and
  MP per stock and fleet, 'MMP' an MP for all stocks and fleets)

- `MPrefs`:

  The names of the MPs applied for each stock (row) and fleet (column).
  An array.

- `nsim`:

  Number of simulations. Single value. Positive integer

- `nstocks`:

  Number of stocks. Single value. Positive integer

- `nfleets`:

  Number of fleets. Single value. Positive integer

- `Snames`:

  Names of the stocks

- `Fnames`:

  Names of the fleets (matrix nstocks x nfleets)

- `Stocks`:

  The stock operating model objects. List of Stocks

- `Fleets`:

  The fleet operating model objects. Hierarchical list, fleets nested in
  stocks.

- `Obss`:

  The fleet specific observation error operating model objects.
  Hierarchical list, fleets nested in stocks.

- `Imps`:

  The fleet specific implementation error operating model objects.
  Hierarchical list, fleets nested in stocks.

- `OM`:

  A table of sampled parameters of the operating model. Data frame of
  nsim rows.

- `Obs`:

  A table of sampled parameters of the observation model. Data frame of
  nsim rows.

- `SB_SBMSY`:

  Simulated spawning biomass relative to SBMSY over the projection. An
  array with dimensions: nsim, nStocks, nMPs, proyears. Non-negative
  real numbers

- `F_FMSY`:

  Simulated fishing mortality rate relative to FMSY over the projection.
  An array with dimensions: nsim, nStocks, nFleets, nMPs, proyears.
  Non-negative real numbers

- `N`:

  Simulated stock numbers over the projection. An array with dimensions:
  nsim, nStocks, `maxage`+1, nMPs, proyears, nareas. Non-negative real
  numbers

- `B`:

  Simulated stock biomass over the projection. An array with dimensions:
  nsim, nStocks, nMPs, proyears. Non-negative real numbers

- `SSB`:

  Simulated spawning stock biomass over the projection. An array with
  dimensions: nsim, nStocks, nMPs, proyears. Non-negative real numbers

- `VB`:

  Simulated vulnerable biomass over the projection. An array with
  dimensions: nsim, nStocks, nMPs, proyears. Non-negative real numbers

- `FM`:

  Simulated fishing mortality rate over the projection. An array with
  dimensions: nsim, nStocks, nFleets, nMPs, proyears. Non-negative real
  numbers

- `SPR`:

  A list of SPR values. Currently not used.

- `Catch`:

  Simulated catches (landings) over the projection. An array with
  dimensions: nsim, nStocks, nFleets, nMPs, proyears. Non-negative real
  numbers

- `Removals`:

  Simulated removals (landings+discards) over the projection. An array
  with dimensions: nsim, nStocks, nFleets, nMPs, proyears. Non-negative
  real numbers

- `Effort`:

  Simulated relative fishing effort in the projection years. An array
  with dimensions: nsim, nStocks, nFleets, nMPs, proyears. Non-negative
  real numbers

- `TAC`:

  Simulated Total Allowable Catch (prescribed) over the projection (this
  is NA for input controls). An array with dimensions: nsim, nStocks,
  nFleets, nMPs, proyears. Non-negative real numbers

- `TAE`:

  Simulated Total Allowable Effort (prescribed) over the projection
  (this is NA for output controls). An array with dimensions: nsim,
  nStocks, nFleets, nMPs, proyears. Non-negative real numbers

- `BioEco`:

  A named list of bio-economic output. Not currently used.

- `RefPoint`:

  Named list of annual MSY reference points MSY, FMSY, and SBMSY. Array
  with dimensions: nsim, nstocks, nMPs, nyears+proyears. Will be the
  same as `multiHist@Ref$ByYear` unless selectivity is changed by MP

- `multiHist`:

  The object of class `multiHist` containing information from the
  spool-up period.

- `PPD`:

  Posterior predictive data. List of `Data` objects at the end of the
  projection period (length `nMPs`)

- `Misc`:

  Miscellaneous output such as posterior predictive data

## Objects from the Class

Objects can be created by calls of the form
`new('MMSE', Name, nyears, proyears, nMPs, MPs, nsim, OMtable, Obs, B_BMSYa, F_FMSYa, Ba, FMa, Ca, OFLa, Effort, PAA, CAA, CAL, CALbins)`

## Author

T. Carruthers
