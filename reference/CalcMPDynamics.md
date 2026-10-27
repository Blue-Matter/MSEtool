# Calculate population dynamics from MP recommendation

An internal function to calculate the population dynamics for the next
time step based on the recent MP recommendation. First uses
`.CalcMPDynamics_SelRetDisc` to update selectivity, retention, and
discarding parameters, then calculates F from effort or catch
recommendations.

## Usage

``` r
CalcMPDynamics(
  MPRecs,
  y,
  nyears,
  proyears,
  nsim,
  Biomass_P,
  VBiomass_P,
  LastTAE,
  histTAE,
  LastSpatial,
  LastAllocat,
  LastTAC,
  TACused,
  maxF,
  LR5_P,
  LFR_P,
  Rmaxlen_P,
  retL_P,
  retA_P,
  retA_P_real,
  retA_P_real_2,
  L5_P,
  LFS_P,
  Vmaxlen_P,
  SLarray_P,
  V_P,
  V_P_real,
  V_P_real_2,
  Fdisc_P,
  DR_P,
  FM_P,
  FM_Pret,
  Z_P,
  CB_P,
  CB_Pret,
  Effort_pot,
  StockPars,
  FleetPars,
  ImpPars,
  checks = FALSE,
  control = list(),
  SelRetBase = NULL
)
```

## Arguments

- MPRecs:

  A named list of MP recommendations. The names are the same as
  `slotNames('Rec')`, except for `Misc`. Each element in the list is a
  matrix. With the exception of `Spatial`, all elements in list have
  `nrow=1` and `ncol=nsim`. `Spatial` has `nrow=nareas`. Matrices can be
  empty matrix, populated with all NAs (both mean no change in
  management with respect to this element (e.g. `Effort`)), or populated
  with a recommendation. MPs must either return a recommendation or no
  recommendation for every simulation for a particular slot (i.e. cannot
  have some NA and some values).

- y:

  The projection year

- nyears:

  The number of historical years

- proyears:

  The number of projection years

- nsim:

  The number of simulations

- Biomass_P:

  An array with dimensions `nsim`, `maxage`, `proyears`, and `nareas`
  with total biomass in the projection years

- VBiomass_P:

  An array with dimensions `nsim`, `maxage`, `proyears`, and `nareas`
  with vulnerable biomass in the projection years

- LastTAE:

  A vector of length `nsim` with the most recent TAE

- LastSpatial:

  A matrix of `nrow=nareas` and `ncol=nsim` with the most recent spatial
  management arrangements

- LastAllocat:

  A vector of length `nsim` with the most recent allocation

- LastTAC:

  A vector of length `nsim` with the most recent TAC

- TACused:

  A vector of length `nsim` with the most recent TAC

- maxF:

  A numeric value with maximum allowed F. From `OM@maxF`

- LR5_P:

  A matrix with `nyears+proyears` rows and `nsim` columns with the first
  length at 5 percent retention.

- LFR_P:

  A matrix with `nyears+proyears` rows and `nsim` columns with the first
  length at full retention.

- Rmaxlen_P:

  A matrix with `nyears+proyears` rows and `nsim` columns with the
  retention at maximum length.

- retL_P:

  An array with dimensions `nsim`, `nCALbins` and `nyears+proyears` with
  retention at length

- retA_P:

  An array with dimensions `nsim`, `maxage` and `nyears+proyears` with
  retention at age

- retA_P_real:

  An array with dimensions `nsim`, `maxage` and `nyears+proyears` with
  realized retention at age. May not asymptote at one

- retA_P_real_2:

  An array with dimensions `nsim`, `maxage` and `nyears+proyears`
  `retA_P_real` standardized to max 1

- L5_P:

  A matrix with `nyears+proyears` rows and `nsim` columns with the first
  length at 5 percent selectivity

- LFS_P:

  A matrix with `nyears+proyears` rows and `nsim` columns with the first
  length at full selectivity

- Vmaxlen_P:

  A matrix with `nyears+proyears` rows and `nsim` columns with the
  selectivity at maximum length.

- SLarray_P:

  An array with dimensions `nsim`, `nCALbins` and `nyears+proyears` with
  selectivity at length

- V_P:

  An array with dimensions `nsim`, `maxage` and `nyears+proyears` with
  selectivity at age

- V_P_real:

  An array with dimensions `nsim`, `maxage` and `nyears+proyears` with
  realized selectivity at age. May not asymptote at one

- V_P_real_2:

  An array with dimensions `nsim`, `maxage` and `nyears+proyears`.
  `V_P_real` standardized to max 1

- Fdisc_P:

  An array with dimensions `nsim`, `maxage` and `nyears+proyears` (or a
  vector of length `nsim`) with discard mortality. From `OM@Fdisc` but
  can be updated by MP (`Rec@Fdisc`)

- DR_P:

  A matrix with `nyears+proyears` rows and `nsim` columns with the
  fraction discarded.

- FM_P:

  An array with dimensions `nsim`, `maxage`, `proyears`, and `nareas`
  with total fishing mortality

- FM_Pret:

  An array with dimensions `nsim`, `maxage`, `proyears`, and `nareas`
  with fishing mortality of the retained fish

- Z_P:

  An array with dimensions `nsim`, `maxage`, `proyears`, and `nareas`
  with total mortality

- CB_P:

  An array with dimensions `nsim`, `maxage`, `proyears`, and `nareas`
  with total catch

- CB_Pret:

  An array with dimensions `nsim`, `maxage`, `proyears`, and `nareas`
  with retained catch

- Effort_pot:

  A numeric vector of potential effort

- StockPars:

  A list of stock parameters

- FleetPars:

  A list of fleet parameters

- ImpPars:

  A list of implementation error parameters

- checks:

  Logical. Run internal checks? Currently not used.

- control:

  List of control parameters used internally.

- SelRetBase:

  List of selectivity-at-length, retention-at-length, harvest slot and
  discard mortality-at-length before accounting for retention and
  discard mortality, returned by the previous call. Created from
  `FleetPars` if `NULL`.

## Value

A named list with updated population dynamics

## See also

[`CalcMPDynamics_MF()`](https://msetool.openmse.com/reference/CalcMPDynamics_MF.md)

## Author

A. Hordyk
