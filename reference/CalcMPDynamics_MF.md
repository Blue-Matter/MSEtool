# Calculate population dynamics from MP recommendation for multi-fleet operating models

An internal function to calculate the population dynamics for the next
time step based on the recent MP recommendation. First uses
`.CalcMPDynamics_SelRetDisc` to update selectivity, retention, and
discarding parameters for individual fleets, then calculates F from
effort or catch recommendations simultaneously across all fleets.

## Usage

``` r
CalcMPDynamics_MF(
  MPRecs_f,
  y,
  nyears,
  proyears,
  nsim,
  LastTAE,
  histTAE,
  LastSpatial,
  LastAllocat,
  LastTAC,
  TACused,
  maxF,
  Effort_pot,
  StockPars,
  FleetPars_f,
  ImpPars_f,
  checks = FALSE,
  control = list()
)
```

## Arguments

- MPRecs_f:

  A nested list first by fleet (length `nf`), then the various MP
  recommendations (`MPRecs`, see
  [`CalcMPDynamics()`](https://msetool.openmse.com/reference/CalcMPDynamics.md)).

- y:

  The projection year

- nyears:

  The number of historical years

- proyears:

  The number of projection years

- nsim:

  The number of simulations

- LastTAE:

  A matrix `[nsim, nf]` with the most recent TAE

- LastSpatial:

  An array `[nareas, nsim, nf]` with the most recent spatial management
  arrangements

- LastAllocat:

  A matrix `[nsim, nf]` with the most recent allocation

- LastTAC:

  A matrix `[nsim, nf]` with the most recent TAC

- TACused:

  A matrix `[nsim, nf]` with the most recent TAC

- maxF:

  A numeric value with maximum allowed F. From `OM@maxF`

- Effort_pot:

  A matrix `[nsim, nf]` of potential effort

- StockPars:

  A list of stock parameters

- FleetPars_f:

  A nested list first by fleet, then each containing the respective
  fleet parameters

- ImpPars_f:

  A nested list first by fleet, then each containing the respective
  implementation error parameters

- checks:

  Logical. Run internal checks? Currently not used.

- control:

  List of control parameters used internally.

## Value

A nested list by fleet with updated population dynamics

## See also

[`CalcMPDynamics()`](https://msetool.openmse.com/reference/CalcMPDynamics.md)

## Author

Q. Huynh
