# Convert ASAP 3 assessments into an operating model

Reads a fitted ASAP model and uses the MLE estimates with identical
reconstruction among simulations. Future recruitment is sampled from a
lognormal distribution with autocorrelation. `ASAP2Data` imports a Data
object.

## Usage

``` r
ASAP2OM(
  asap,
  nsim = 48,
  proyears = 50,
  mcmc = FALSE,
  spawn_time_frac = 0,
  Name = "ASAP Model",
  Source = "No source provided",
  nyr_par_mu = 3,
  Author = "No author provided",
  report = FALSE,
  silent = FALSE
)

ASAP2Data(asap, Name = "ASAP assessment")
```

## Arguments

- asap:

  A list returned by ASAP, e.g., `asap <- dget("asap3.rdat")`.

- nsim:

  The number of simulations in the operating model

- proyears:

  The number of MSE projection years

- mcmc:

  Logical, whether to use mcmc samples. Currently unsupported.

- spawn_time_frac:

  Numeric, the fraction of a year when spawning takes place (e.g., 0.5
  is the midpoint of the year)

- Name:

  The name of the operating model

- Source:

  Reference to assessment documentation e.g. a url

- nyr_par_mu:

  integer, the number of recent years to estimate vulnerability over for
  future projections

- Author:

  Who did the assessment

- report:

  Logical, should a comparison of biomass reconstruction be produced?

- silent:

  Logical, should progress reporting be printed to the console?

## Value

An operating model
[OM](https://msetool.openmse.com/reference/OM-class.md) class.

## Details

Length at age is not used in ASAP so arbitrary placeholder values are
used for length-based parameters. Update these parameters to model
length in the operating model.

## See also

[Assess2OM](https://msetool.openmse.com/reference/Assess2OM.md)

## Author

Q. Huynh
