# Valid custom parameters (cpars)

Valid custom parameters (cpars)

## Usage

``` r
validcpars(
  type = c("all", "Stock", "Fleet", "Obs", "Imp", "internal"),
  valid = TRUE,
  show = TRUE
)
```

## Arguments

- type:

  What cpars to show? 'all', 'Stock', 'Fleet', 'Obs', 'Imp', or
  'internal'

- valid:

  Logical. Show valid cpars?

- show:

  Logical. Display the table in the Viewer?

## Value

a HTML datatable with variable name, description and type of valid cpars

## Control list

A named list for `control`, for example,
`OM@cpars$control <- list(TAC = "retained", CAL = "removals")`, can be
specified to override default settings in the MSE simulation. Possible
names in the `control` list are:

- `TAC` Character, the catch that the TAC applies to. `"removals"`
  (default) applies the TAC to the retained catch plus dead discards.
  `"retained"` applies the TAC to the retained catch only, so that
  removals exceed the TAC when there are dead discards.

- `CAL` Character, set to `"removals"` to sample the catch-at-length
  from retained + discarded catch. Default only samples from retained
  catch.

- `D` Character, set to `"VB"` so that historical depletion `OM@D`
  corresponds to vulnerable biomass depletion (only used when
  `OM@cpars$qs = NULL`).

- `optVB` Logical, set to `TRUE` so that historical depletion `OM@D`
  corresponds to vulnerable biomass depletion. Default sets depletion
  according to spawning biomasss when `OM@cpars$qs = NULL`.

- `optSBMSY` Logical, set to `TRUE` such that `OM@D` corresponds to the
  ratio of spawning biomass to MSY. Default uses according to spawning
  biomass depletion (biomass relative to unfished levels).

- `Depletion` Character, set to `"end"` such that historical depletion
  `OM@D` corresponds to the biomass at the end of the last projection
  year. Default corresponds to the value at the beginning of the last
  projection year.

- `ntrials` Integer, set the number of iterations to sample the
  operating model to match the depletion to `OM@D`. Default is 50.

- `fracD` Numeric, the maximum allowable proportion of simulations
  allowed to hit the bounds of the depletion parameter (simulation
  returns an error if exceeded). Default is 0.05.

- `checks` Logical. If `TRUE`, plots depletion and SB/SBMSY figures and
  prints values to the R console to diagnose issues with operating model
  configuration with regards to depletion.

- `unfished` Logical. If `TRUE`, returns historical simulations with F =
  0.

- `progress` Logical. If `TRUE`, updates progress bar through
  [`shiny::incProgress`](https://rdrr.io/pkg/shiny/man/withProgress.html).
  Used in conjunction with Shiny apps.

- `maxiterF` Integer, the number of iterations to solve for F in the
  projections from the specified TAC. Default is 300.

- `tolF` Numeric, the tolerance for the catch relative to the TAC when
  solving for F in the projections. Default is 1e-4.

- `HZN` Integer, the number of generations to solve for B_low. Default
  is 2. See
  [`getBlow()`](https://msetool.openmse.com/reference/getBlow.md).

- `Bfrac` Numeric, proportion of SBMSY to solve for B_low. Default is
  0.5. See
  [`getBlow()`](https://msetool.openmse.com/reference/getBlow.md).

- `skipdata` Logical. If `TRUE`, skips conditioning on data in
  `MOM@cpars[[p]][[f]]$Data`. Only used in
  [`multiMSE()`](https://msetool.openmse.com/reference/multiMSE.md).

- `HermEq` Logical, whether the equilibrium population age structures in
  the multi-OM is generated from the hermaphroditism vector (intended
  for use in salmonMSE). Default is TRUE. Only used in
  [`multiMSE()`](https://msetool.openmse.com/reference/multiMSE.md).

- `HistRel` Logical, whether to perform the historical reconstruction
  with inter-stock relationships in `MOM@Rel`. Default is TRUE. Only
  used in
  [`multiMSE()`](https://msetool.openmse.com/reference/multiMSE.md).

## Examples

``` r
if (FALSE) { # \dontrun{
validcpars() # all valid cpars

validcpars("Obs", FALSE) # invalid Obs cpars
} # }
```
