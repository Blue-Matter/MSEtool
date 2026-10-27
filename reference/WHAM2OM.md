# Takes a fitted SAM model and samples historical population and fishing dynamics from the MLE fit and variance-covariance matrix.

Takes a fitted SAM model and samples historical population and fishing
dynamics from the MLE fit and variance-covariance matrix.
Maturity-at-age-year, Mortality-at-age-year and weight-at-age-year are
identical among simulations and are a direct copy of the matrices in the
WHAM fitting object.

## Usage

``` r
WHAM2OM(
  obj,
  nsim = 3,
  proyears = 30,
  interval = 2,
  Name = NULL,
  WLa = 1,
  WLb = 3,
  WAAind = 1,
  Obs = MSEtool::Imprecise_Unbiased,
  Imp = MSEtool::Perfect_Imp,
  nyr_par_mu = 3,
  LowerTri = 2,
  plusgroup = T,
  altinit = 0,
  fixq1 = T,
  report = FALSE,
  silent = FALSE,
  ...
)
```

## Arguments

- obj:

  a SAM output object

- nsim:

  Positive integer. The number of simulations.

- proyears:

  Positive integer. The number of projection years for MSE.

- interval:

  Positive integer. The interval at which management procedures will
  update the management advice in
  [runMSE](https://msetool.openmse.com/reference/runMSE.md), e.g., 1 =
  annual updates.

- Name:

  Character string. The name of the operating model.

- WLa:

  positive real number or array `[sim, ages, year]`. The default
  weight-length parameter a (W=aL^b)

- WLb:

  positive real number or array `[sim, ages, year]`. The default
  weight-length parameter b (W=aL^b)

- WAAind:

  positive integer. The index of the WHAM weight-at-age array
  input\$data\$waa to be assumed as the weight-at-age for the operating
  model

- Obs:

  The observation model (class Obs). This function only updates the
  catch and index observation error.

- Imp:

  The implementation model (class Imp). This function does not update
  implementation parameters.

- nyr_par_mu:

  Positive integer. The number of recent years that natural mortality,
  age vulnerability, weight, length and maturity parameters are averaged
  over for defining future projection conditions.

- LowerTri:

  Integer. The number of recent years for which model estimates of
  recruitment are ignored (not reliably estimated by the assessment)

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

  Additional arguments, including R0 (unfished recruitment), phi0
  (unfished spawners per recruit associated with R0 and h for
  calculating stock recruit parameters),

## Value

An object of class
[OM](https://msetool.openmse.com/reference/OM-class.md).

## Details

Use a seed for the random number generator to sample future recruitment.

## See also

[Assess2OM](https://msetool.openmse.com/reference/Assess2OM.md)

## Author

T. Carruthers
