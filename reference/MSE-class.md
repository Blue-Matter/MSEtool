# Class `'MSE'`

A Management Strategy Evaluation object that contains information about
simulation conditions and performance of data-limited methods

## Slots

- `Name`:

  Name of the MSE object. Single value. Character string

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

- `nsim`:

  Number of simulations. Single value. Positive integer

- `OM`:

  Operating model parameters (last historical year used for time-varying
  parameters). Data.frame with `nsim` rows

- `Obs`:

  Observation parameters (last historical year used for time-varying
  parameters). Data.frame with `nsim` rows

- `SB_SBMSY`:

  Simulated spawning biomass relative to spawning BMSY over the
  projection. An array with dimensions: nsim, nMPs, proyears.
  Non-negative real numbers

- `F_FMSY`:

  Simulated fishing mortality rate relative to FMSY over the projection.
  An array with dimensions: nsim, nMPs, proyears. Non-negative real
  numbers

- `N`:

  Simulated total numbers over the projection. An array with dimensions:
  nsim, maxage+1, nMPs, proyears, nareas. Non-negative real numbers.

- `B`:

  Simulated stock biomass over the projection. An array with dimensions:
  nsim, nMPs, proyears. Non-negative real numbers

- `SSB`:

  Simulated spawning stock biomass over the projection. An array with
  dimensions: nsim, nMPs, proyears. Non-negative real numbers

- `VB`:

  Simulated vulnerable biomass over the projection. An array with
  dimensions: nsim, nMPs, proyears. Non-negative real numbers

- `FM`:

  Simulated fishing mortality rate over the projection. An array with
  dimensions: nsim, nMPs, proyears. Non-negative real numbers

- `SPR`:

  Named list with equilibrium and dynamic SPR. Each element is an array
  with dimensions: nsim, nMPs, proyears. Non-negative real numbers.

- `Catch`:

  Simulated catches (landings) over the projection. An array with
  dimensions: nsim, nMPs, proyears. Non-negative real numbers

- `Removals`:

  Simulated removals (catch + discards) over the projection. An array
  with dimensions: nsim, nMPs, proyears. Non-negative real numbers

- `Effort`:

  Simulated relative fishing effort in the projection years. An array
  with dimensions: nsim, nMPs, proyears. Non-negative real numbers

- `TAC`:

  Simulated Total Allowable Catch prescribed by MPs. An array with
  dimensions: nsim, nMPs, proyears. Non-negative real numbers

- `TAE`:

  Simulated Total Allowable Effort prescribed by MPs. An array with
  dimensions: nsim, nMPs, proyears. Non-negative real numbers

- `BioEco`:

  Named list with bio-economic output Only used if bio-economic
  parameters are included in OM

- `RefPoint`:

  Named list of annual MSY reference points MSY, FMSY, and SBMSY. Array
  with dimensions: nsim, nMPs, nyears+proyears. Will be the same as
  `Hist@Ref$ByYear` unless selectivity is changed by MP

- `CB_hist`:

  Simulated catches (landings) from the spool-up period. An array with
  dimensions: nsim, nyears. Non-negative real numbers

- `FM_hist`:

  Simulated fishing mortality rate from the spool-up period. An array
  with dimensions: nsim, nyears Non-negative real numbers

- `SSB_hist`:

  Simulated spawning stock biomass from the spool-up period. An array
  with dimensions: nsim, nyears. Non-negative real numbers

- `Hist`:

  Information from the historical spool-up period. Object of class
  `Hist`. Only contains slots `AtAge` and `TSdata` unless
  `extended=TRUE` in `runMSE`

- `PPD`:

  Posterior predictive data. List of `Data` objects at the end of the
  projection period (length `nMPs`)

- `Misc`:

  Miscellaneous output

## Author

T. Carruthers and A. Hordyk
