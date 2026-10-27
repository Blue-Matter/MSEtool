# Run a Management Strategy Evaluation

Functions to run the Management Strategy Evaluation (closed-loop
simulation) for a specified operating model

## Usage

``` r
Simulate(OM = MSEtool::testOM, parallel = FALSE, silent = FALSE, nsim = NULL)

Project(
  Hist = NULL,
  MPs = NA,
  parallel = FALSE,
  silent = FALSE,
  extended = FALSE,
  checkMPs = FALSE
)

runMSE(
  OM = MSEtool::testOM,
  MPs = NA,
  Hist = FALSE,
  silent = FALSE,
  parallel = FALSE,
  extended = FALSE,
  checkMPs = FALSE
)
```

## Arguments

- OM:

  An operating model object (class
  [OM](https://msetool.openmse.com/reference/OM-class.md) or class
  `Hist`). Also works for `MOM` objects, as a wrapper for `ProjectMOM`

- parallel:

  Logical or a named list. Should MPs be run using parallel processing?
  For `runMSE`, can also be `"sac"` to run the entire MSE in parallel
  using the split-apply-combine technique. See Details for more
  information.

- silent:

  Should messages be printed out to the console?

- nsim:

  Optional. numeric value to override `OM@nsim`.

- Hist:

  Should model stop after historical simulations? Returns an object of
  class 'Hist' containing all historical data

- MPs:

  A vector of methods (character string) of class MP

- extended:

  Logical. Return extended projection results? if TRUE,
  `MSE@Misc$extended` is a named list with extended data (including
  historical and projection by area), extended version of `MSE@Hist` is
  returned, and returns `MSE@PPD` with StockPars, FleetPars, and
  ReferencePoints in `MSE@PPD`

- checkMPs:

  Logical. Check if the specified MPs exist and can be run on
  `SimulatedData`?

## Value

Functions return objects of class
[Hist](https://msetool.openmse.com/reference/Hist-class.md) or
[MSE](https://msetool.openmse.com/reference/MSE-class.md)

- Simulate - An object of class
  [Hist](https://msetool.openmse.com/reference/Hist-class.md)

- Project - An object of class
  [MSE](https://msetool.openmse.com/reference/MSE-class.md)

- runMSE - An object of class
  [MSE](https://msetool.openmse.com/reference/MSE-class.md) if
  `Hist = TRUE` otherwise a class
  [Hist](https://msetool.openmse.com/reference/Hist-class.md) object

## Details

### Running MPs in parallel

For most MPs, running in parallel can actually lead to an increase in
computation time, due to the overhead in sending the information over to
the cores. Consequently, by default the MPs will not be run in parallel
if `parallel=TRUE` (although other internal code will be run in parallel
mode).

To run MPs in parallel, specify a named list with the name of the MP(s)
assigned as TRUE. For example,`parallel=list(AvC=TRUE`) will run the
`AvC` MP in parallel mode.

### Split-apply-combine MSE in parallel

Additional savings in computation time can be achieved by running the
entire simulation in batches. Individual simulations of the operating
model are divided into separate cores using
[SubCpars](https://msetool.openmse.com/reference/SubCpars.md),
`Simulate` and `Project` are applied independently for each core via
[`snowfall::sfClusterApplyLB`](https://rdrr.io/pkg/snowfall/man/snowfall-c-calculation.html),
and the output (a list of MSE objects) is stitched back together into a
single MSE object using
[joinMSE](https://msetool.openmse.com/reference/checkMSE.md).

The ideal number of cores will be determined based on the number of
simulations and available cores.

There are several issues to look out for when using this
split-apply-combine technique:

- Numerical optimization for depletion may fail in individual cores when
  `OM@cpars$qs` is not specified.

- Length bins should be specified in the operating model in
  `OM@cpars$CAL_bins`. Otherwise, length bins can vary by core and
  create problems when combining into a single object.

- Compared to non-parallel runs, sampled parameters in the operating
  model will vary despite the same value in `OM@seed`.

- If there is an error in individual cores or while combining the
  parallel output into a single Hist or MSE object, the list of output
  (from the cores) will be returned.

## Functions

- `Simulate()`: Run the Historical Simulations from an object of class
  `OM`

- `Project()`: Run the Forward Projections

- `runMSE()`: Run the Historical Simulations and Forward Projections
  from an object of class \`OM
