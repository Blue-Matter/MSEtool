# Reads MPD or MCMC estimates and data from iSCAM file structure into an operating model

Functions for importing an iSCAM assessment. From a fitted model,
`iSCAM2OM` populates the various slots of an operating model and
`iSCAM2Data` generates a Data object. These functions rely on several
functions written by Chris Grandin (DFO PBS).

## Usage

``` r
iSCAM2OM(
  iSCAMdir,
  nsim = 48,
  proyears = 50,
  mcmc = FALSE,
  spawn_time_frac = 0,
  Name = "iSCAM model",
  Source = "No source provided",
  length_timestep = 1,
  nyr_par_mu = 2,
  Author = "No author provided",
  report = FALSE,
  silent = FALSE
)

iSCAM2Data(
  iSCAMdir,
  Name = "iSCAM assessment",
  Source = "No source provided",
  length_timestep = 1,
  Author = "No author provided"
)
```

## Arguments

- iSCAMdir:

  A folder with iSCAM input and output files in it. Alternatively, a
  list returned by
  [load.iscam.files](https://msetool.openmse.com/reference/iSCAM.md).

- nsim:

  The number of simulations to take for parameters with uncertainty (for
  OM@cpars custom parameters)

- proyears:

  The number of MSE projection years

- mcmc:

  Logical, whether to use mcmc samples to create custom parameters
  cpars. Alternatively, a list returned by
  [read.mcmc](https://msetool.openmse.com/reference/iSCAM.md). Set the
  seed for the function to sub-sample the mcmc samples.

- spawn_time_frac:

  Numeric between 0-1 indicating when spawning occurs within the time
  step

- Name:

  The name of the operating model

- Source:

  Reference to assessment documentation e.g. a url

- length_timestep:

  How long is a model time step in years (e.g. a quarterly model is
  0.25, a monthly model 1/12) (currently only uses annual time step)

- nyr_par_mu:

  integer, the number of recent years to estimate vulnerability over for
  future projections

- Author:

  Who did the assessment

- report:

  logical should a numbers at age reconstruction plot be produced?

- silent:

  logical should progress reporting be printed to the console?

## Biological parameters

The function calls `model <- load.iscam.files(iSCAMdir)` and grabs the
following matrices:

- `model$mpd$d3_weight_mat` - fecundity (product of weight and maturity
  at age)

- `model$mpd$ma` - maturity at age

## MPD historical reconstruction

The function calls `model <- load.iscam.files(iSCAMdir)` and then grabs
the following matrices:

- `model$mpd$N` - abundance at age

- `model$mpd$F` - fishing mortality at age

- `model$mpd$M` - natural mortality at age

If a delay-difference model is recognized, then the following is used
instead:

- `model$mpd$F_dd` - fishing mortality at age

- `model$mpd$M_dd` - natural mortality at age

Abundance at age is reconstructed using `model$mpd$rt` (recruitment) and
projected with `F_dd` and `M_dd` to match `model$mpd$numbers`.

## MCMC historical reconstruction

If `mcmc = TRUE` the function calls `mcmc_model <- read.mcmc(iSCAMdir)`,
and grabs `nsim` sub-samples of the MCMC output through the following
arrays:

- `mcmc_model$params` and `mcmc_model$ft` - fishing mortality at age
  from the fleet selectivity parameters and apical F's

- `mcmc_model$m` - year-specific natural mortality at age

- `mcmc_model$params$rinit` and `mcmc_model$rt` - recruitment and
  abundance

## Start age

While the iSCAM start age can be greater than zero, abundance at age is
back-calculated to age zero with M, maturity, growth = 0. In this way,
the stock-recruit dynamics from iSCAM are preserved.

These arrays are then passed to
[Assess2OM](https://msetool.openmse.com/reference/Assess2OM.md) to
generate the operating model.

## Reference points

iSCAM calculates the stock-recruit relationship and subsequently a
single set of MSY and unfished reference points using R0, steepness, and
unfished spawners per recruit from the mean M, fecundity, and growth
(mean with respect to time).

R0 and h are recalculated for the operating model by obtaining the
stock-recruit alpha and beta from the iSCAM parameters and the mean
unfished spawners per recruit in the first `ageM` (age of 50% maturity)
years.

## Author

T. Carruthers, Q. Huynh
