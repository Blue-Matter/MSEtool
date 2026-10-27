# Class `'Data'`

An object for storing fishery data for analysis

## Slots

- `Name`:

  The name of the Data object. Single value. Character string

- `Common_Name`:

  Common name of the species. Character string

- `Species`:

  Scientific name of the species. Genus and species name. Character
  string

- `Region`:

  Name of the general geographic region of the fishery. Character string

- `LHYear`:

  The last historical year of the simulation (before projection). Single
  value. Positive integer

- `MPrec`:

  The previous recommendation of a management procedure. Vector of
  length nsim. Positive real numbers

- `Units`:

  Units of the catch/absolute abundance estimates. Single value.
  Character string

- `MPeff`:

  The current level of effort. Vector of length nsim. Positive real
  numbers

- `nareas`:

  Number of fishing areas. Vector of length nsim. Non-negative integer

- `MaxAge`:

  Maximum age. Vector nsim long. Positive integer

- `Mort`:

  Natural mortality rate. Vector nsim long. Positive real numbers

- `CV_Mort`:

  Coefficient of variation in natural mortality rate. Vector nsim long.
  Positive real numbers

- `vbLinf`:

  Maximum length. Vector nsim long. Positive real numbers

- `CV_vbLinf`:

  Coefficient of variation in maximum length. Vector nsim long. Positive
  real numbers

- `vbK`:

  The von Bertalanffy growth coefficient K. Vector nsim long. Positive
  real numbers

- `CV_vbK`:

  Coefficient of variation in the von Bertalanffy K parameter. Vector
  nsim long. Positive real numbers

- `vbt0`:

  Theoretical age at length zero. Vector nsim long. Non-positive real
  numbers

- `CV_vbt0`:

  Coefficient of variation in age at length zero. Vector nsim long.
  Positive real numbers

- `wla`:

  Weight-Length parameter alpha. Vector nsim long. Positive real numbers

- `CV_wla`:

  Coefficient of variation in weight-length parameter a. Vector nsim
  long. Positive real numbers

- `wlb`:

  Weight-Length parameter beta. Vector nsim long. Positive real numbers

- `CV_wlb`:

  Coefficient of variation in weight-length parameter b. Vector nsim
  long. Positive real numbers

- `steep`:

  Steepness of stock-recruitment relationship. Vector nsim long. Value
  in the range of one-fifth to 1

- `CV_steep`:

  Coefficient of variation in steepness. Vector nsim long. Positive real
  numbers

- `sigmaR`:

  Recruitment variability. Vector nsim long. Positive real numbers

- `CV_sigmaR`:

  Coefficient of variation in recruitment variability. Vector nsim long.
  Positive real numbers

- `L50`:

  Length at 50 percent maturity. Vector nsim long. Positive real numbers

- `CV_L50`:

  Coefficient of variation in length at 50 per cent maturity. Vector
  nsim long. Positive real numbers

- `L95`:

  Length at 95 percent maturity. Vector nsim long. Positive real numbers

- `LenCV`:

  Coefficient of variation of length-at-age (assumed constant for all
  age classes). Vector nsim long. Positive real numbers

- `LFC`:

  Length at first capture. Vector nsim long. Positive real numbers

- `CV_LFC`:

  Coefficient of variation in length at first capture. Vector nsim long.
  Positive real numbers

- `LFS`:

  Shortest length at full selection. Vector nsim long. Positive real
  numbers

- `CV_LFS`:

  Coefficient of variation in length at full selection. Vector nsim
  long. Positive real numbers

- `Vmaxlen`:

  Vulnerability of individuals at asymptotic length. Vector nsim long.
  Real number between 0 and 1.

- `Year`:

  Years that corresponding to catch and relative abundance data. Vector
  nyears long. Positive integer

- `Cat`:

  Total annual catches. Matrix of nsim rows and nyears columns.
  Non-negative real numbers

- `CV_Cat`:

  Coefficient of variation in annual catches. Matrix nsim rows and
  either 1 or nyear columns. Positive real numbers. Note: built-in MPs
  use only the first value of `CV_Cat` for all years.

- `Effort`:

  Annual fishing effort. Matrix of nsim rows and nyears columns.
  Non-negative real numbers

- `CV_Effort`:

  Coefficient of variation in annual effort. Matrix nsim rows and either
  1 or nyear columns. Positive real numbers. Note: built-in MPs use only
  the first value of `CV_Effort` for all years.

- `Ind`:

  Relative total abundance index. Matrix of nsim rows and nyears
  columns. Non-negative real numbers

- `CV_Ind`:

  Coefficient of variation in the relative total abundance index. Matrix
  nsim rows and either 1 or nyear columns. Positive real numbers. Note:
  built-in MPs use only the first value of `CV_Ind` for all years

- `SpInd`:

  Relative spawning abundance index. Matrix of nsim rows and nyears
  columns. Non-negative real numbers

- `CV_SpInd`:

  Coefficient of variation in the relative spawning abundance index.
  Matrix nsim rows and either 1 or nyear columns. Positive real numbers.

- `VInd`:

  Relative vulnerable abundance index. Matrix of nsim rows and nyears
  columns. Non-negative real numbers

- `CV_VInd`:

  Coefficient of variation in the relative vulnerable abundance index.
  Matrix nsim rows and either 1 or nyear columns. Positive real numbers.

- `AddInd`:

  Optional additional indices. Array of dimensions `nsim`, n additional
  indices, and `nyears` (length `Year`).

- `CV_AddInd`:

  Coefficient of variation for additional indices. Array of same
  dimensions as `AddInd`

- `AddIndV`:

  Vulnerability-at-age schedules for the additional indices. Array with
  dimensions: `nsim`, n additional indices, `MaxAge+1`.

- `AddIunits`:

  Units for the additional indices - biomass (1; default) or numbers
  (0). Numeric vector length n.ind.

- `AddIndType`:

  Index calculated from total stock (1, default), spawning stock (2), or
  vulnerable stock (3). Numeric vector of length n.ind

- `Rec`:

  Recent recruitment strength. Matrix of nsim rows and nyears columns.
  Non-negative real numbers

- `CV_Rec`:

  Log-normal CV for recent recruitment strength. Matrix nsim rows and
  either 1 or nyear columns. Positive real numbers. Note: built-in MPs
  use only the first value of `CV_Rec` for all years.

- `ML`:

  Mean length time series. Matrix of nsim rows and nyears columns.
  Non-negative real numbers

- `Lc`:

  Modal length of catches. Matrix of nsim rows and nyears columns.
  Positive real numbers

- `Lbar`:

  Mean length of catches over Lc. Matrix of nsim rows and nyears
  columns. Positive real numbers

- `Vuln_CAA`:

  Optional vulnerability-at-age schedule for catch-at-age samples. Used
  to condition OM for closed-loop simulation testing. Replaces the fleet
  selectivity schedule in the OM used to generate CAA samples. Matrix
  with dimensions `nsim` x `MaxAge+1`.

- `CAA`:

  Catch at Age data (numbers). Array of dimensions nsim x nyears x
  MaxAge+1. Non-negative integers

- `Vuln_CAL`:

  Optional vulnerability-at-length schedule for catch-at-length samples.
  Used to condition OM for closed-loop simulation testing. Replaces the
  fleet selectivity schedule in the OM used to generate CAL samples.
  Matrix with dimensions `nsim` x `length(CAL_mids)`.

- `CAL_bins`:

  The values delimiting the length bins for the catch-at-length data.
  Vector. Non-negative real numbers

- `CAL_mids`:

  The values of the mid-points of the length bins. Optional, calculated
  from `CAL_bins` if not entered. Vector. Non-negative real numbers.

- `CAL`:

  Catch-at-length data. An array with dimensions nsim x nyears x
  length(CAL_mids). Non-negative integers. By default the CAL data will
  be the retained lengths (i.e, not including discards). If
  `OM@control$CAL =="removals"` then the CAL data will include all
  removals (retained + discards).

- `Dep`:

  Stock depletion SSB(current)/SSB(unfished). Vector nsim long.
  Fraction.

- `CV_Dep`:

  Coefficient of variation in current stock depletion. Vector nsim long.
  Positive real numbers

- `Abun`:

  An estimate of absolute current vulnerable abundance. Vector nsim
  long. Positive real numbers

- `CV_Abun`:

  Coefficient of variation in estimate of absolute current stock size.
  Vector nsim long. Positive real numbers

- `SpAbun`:

  An estimate of absolute current spawning stock abundance. Vector nsim
  long. Positive real numbers

- `CV_SpAbun`:

  Coefficient of variation in estimate of absolute spawning current
  stock size. Vector nsim long. Positive real numbers

- `FMSY_M`:

  An assumed ratio of FMSY to M. Vector nsim long. Positive real numbers

- `CV_FMSY_M`:

  Coefficient of variation in the ratio in FMSY/M. Vector nsim long.
  Positive real numbers

- `BMSY_B0`:

  The most productive stock size relative to unfished. Vector nsim long.
  Fraction

- `CV_BMSY_B0`:

  Coefficient of variation in the position of the most productive stock
  size relative to unfished. Vector nsim long. Positive real numbers

- `Cref`:

  Reference or target catch level (eg MSY). Vector of length nsim.
  Positive real numbers

- `CV_Cref`:

  Log-normal CV for reference or target catch level. Vector of length
  nsim. Positive real numbers

- `Bref`:

  Reference or target biomass level (eg BMSY). Vector of length nsim.
  Positive real numbers

- `CV_Bref`:

  Log-normal CV for reference or target biomass level. Vector of length
  nsim. Positive real numbers

- `Iref`:

  Reference or target relative abundance index level (eg BMSY / B0).
  Vector of length nsim. Positive real numbers

- `CV_Iref`:

  Log-normalCV for reference or target relative abundance index level.
  Vector of length nsim. Positive real numbers

- `t`:

  The number of years corresponding to AvC and Dt. Single value.
  Positive integer

- `AvC`:

  Average catch over time t. Vector nsim long. Positive real numbers

- `CV_AvC`:

  Coefficient of variation in average catches over time t. Vector nsim
  long. Positive real numbers

- `Dt`:

  Depletion over time t SSB(now)/SSB(now-t+1). Vector nsim long.
  Fraction

- `CV_Dt`:

  Coefficient of variation in depletion over time t. Vector nsim long.
  Positive real numbers

- `Ref`:

  A reference management level (eg a catch limit). Single value.
  Positive real number

- `Ref_type`:

  Type of reference management level (eg 2009 catch limit). Single
  value. Character string

- `Log`:

  A record of events. Single value. Character string

- `params`:

  A place to store estimated parameters. An object. R list

- `PosMPs`:

  The methods that can be applied to these data. Vector. Character
  strings

- `TAC`:

  The calculated catch limits (function TAC). An array with dimensions
  PosMPs x replicate TAC samples x nsim. Positive real numbers

- `Sense`:

  The results of the sensitivity analysis (function Sense). An array
  with dimensions PosMPs x sensitivity increments. Positive real numbers

- `MPs`:

  The methods that were applied to these data. Vector. Character strings

- `OM`:

  A table of operating model conditions. R table object of nsim rows.
  Real numbers

- `Obs`:

  A table of observation model conditions. R table object of nsim rows.
  Real numbers

- `Misc`:

  Other information for MPs. An object. R list

## Objects from the Class

Objects can be created by calls of the form `new('Data', stock)`

## Author

T. Carruthers and A. Hordyk

## Examples

``` r

newdata<-new('Data')
```
