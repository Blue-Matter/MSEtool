# Reads bootstrap estimates from a stock assessment model (including VPA) into an operating model. `Assess2OM` is identical to `VPA2OM`.

A function that uses a set of bootstrap estimates of numbers-at-age,
fishing mortality rate-at-age, M-at-age, weight-at-age, length-at-age
and Maturity-at-age to define a fully described MSEtool operating model.
The user still needs to parameterize most of the observation and
implementation portions of the operating model.

## Usage

``` r
Assess2OM(
  Name = "A fishery made by VPA2OM",
  proyears = 50,
  interval = 2,
  CurrentYr = as.numeric(format(Sys.Date(), "%Y")),
  h = 0.999,
  Obs = MSEtool::Imprecise_Unbiased,
  Imp = MSEtool::Perfect_Imp,
  naa,
  faa,
  waa,
  Mataa,
  Maa,
  laa,
  nyr_par_mu = 3,
  LowerTri = 1,
  recind = 0,
  plusgroup = TRUE,
  altinit = 0,
  fixq1 = TRUE,
  report = FALSE,
  silent = FALSE,
  ...
)

VPA2OM(
  Name = "A fishery made by VPA2OM",
  proyears = 50,
  interval = 2,
  CurrentYr = as.numeric(format(Sys.Date(), "%Y")),
  h = 0.999,
  Obs = MSEtool::Imprecise_Unbiased,
  Imp = MSEtool::Perfect_Imp,
  naa,
  faa,
  waa,
  Mataa,
  Maa,
  laa,
  nyr_par_mu = 3,
  LowerTri = 1,
  recind = 0,
  plusgroup = TRUE,
  altinit = 0,
  fixq1 = TRUE,
  report = FALSE,
  silent = FALSE,
  ...
)
```

## Arguments

- Name:

  Character string. The name of the operating model.

- proyears:

  Positive integer. The number of projection years for MSE.

- interval:

  Positive integer. The interval at which management procedures will
  update the management advice in
  [runMSE](https://msetool.openmse.com/reference/runMSE.md), e.g., 1 =
  annual updates.

- CurrentYr:

  Positive integer. The current year (final year of fitting to data)

- h:

  The steepness of the stock-recruitment curve (greater than 0.2 and
  less than 1, assumed to be close to 1 to match VPA assumption). Either
  a single numeric or a length nsim vector.

- Obs:

  The observation model (class Obs). This function only updates the
  catch and index observation error.

- Imp:

  The implementation model (class Imp). This function does not update
  implementation parameters.

- naa:

  Numeric array `[sim, ages, year]`. Numbers-at-age
  `[first age is age zero]`.

- faa:

  Numeric array `[sim, ages, year]`. Fishing mortality rate-at-age
  `[first age is age zero]`.

- waa:

  Numeric array `[sim, ages, year]`. Weight-at-age
  `[first age is age zero]`.

- Mataa:

  Numeric array `[sim, ages, year]`. Maturity (spawning fraction)-at-age
  `[first age is age zero]`.

- Maa:

  Numeric array `[sim, ages, year]`. Natural mortality rate-at-age
  `[first age is age zero]`.

- laa:

  Numeric array `[sim, ages, year]`. Length-at-age
  `[first age is age zero]`.

- nyr_par_mu:

  Positive integer. The number of recent years that natural mortality,
  age vulnerability, weight, length and maturity parameters are averaged
  over for defining future projection conditions.

- LowerTri:

  Integer. The number of recent years for which model estimates of
  recruitment are ignored (not reliably estimated by the assessment)

- recind:

  Positive integer. The first age class that fish 'recruit to the
  fishery'. The default is 0 - ie the first position in the age
  dimension of naa is age zero

- plusgroup:

  Logical. Does the assessment assume that the oldest age class is a
  plusgroup?

- altinit:

  Integer. Various assumptions for how to set up the initial numbers. 0:
  standard, 1: no plus group, 2: temporary fix for MSEtool plus group
  initialization

- fixq1:

  Logical. Should q be fixed (ie assume the F-at-age array faa is
  accurate?

- report:

  Logical, if TRUE, a diagnostic will be reported showing the matching
  of the OM reconstructed numbers at age vs the assessment.

- silent:

  Whether to silence messages to the console.

- ...:

  Additional arguments (for all, either a numeric or a length nsim
  vector):

  - `fecaa` Fecundity at age. Default fecundity is the product of
    maturity and weight at age.

  - `SRrel` Stock-recruit relationship. (`1` for Beverton-Holt
    (default), `2` for Ricker)

  - `R0` unfished recruitment

  - `phi0` unfished spawners per recruit associated with R0 and h. With
    time-varying parameters, openMSE uses the mean phi0 in the first
    `ageM` (age of 50 percent maturity) years for the stock-recruit
    relationship. `Assess2OM` will re-calculate R0 and h in the
    operating model such that the stock-recruit `alpha` and `beta`
    parameters match values implied in the input.

  - `Perr` recruitment standard deviation (lognormal distribution) for
    sampling future recruitment

  - `AC` autocorrelation in future recruitment deviates.

  - `spawn_time_frac` The fraction of a year when spawning takes place
    (e.g., 0.5 is the midpoint of the year)

## Value

An object of class
[OM](https://msetool.openmse.com/reference/OM-class.md).

## Details

Use a seed for the random number generator to sample future recruitment.

## See also

[SS2OM](https://msetool.openmse.com/reference/SS2MOM.md)
[iSCAM2OM](https://msetool.openmse.com/reference/iSCAM2OM.md)
[WHAM2OM](https://msetool.openmse.com/reference/WHAM2OM.md)
[ASAP2OM](https://msetool.openmse.com/reference/ASAP2OM.md)

## Author

T. Carruthers
