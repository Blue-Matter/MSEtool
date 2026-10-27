# Run a multi-fleet multi-stock Management Strategy Evaluation

Functions for running a multi-stock and/or multi-fleet Management
Strategy Evaluation (closed-loop simulation) for a specified operating
model

## Usage

``` r
SimulateMOM(MOM = MSEtool::Albacore_TwoFleet, parallel = TRUE, silent = FALSE)

ProjectMOM(
  multiHist = NULL,
  MPs = NA,
  parallel = FALSE,
  silent = FALSE,
  checkMPs = FALSE,
  dropHist = FALSE,
  extended = FALSE
)

multiMSE(
  MOM = MSEtool::Albacore_TwoFleet,
  MPs = list(list(c("AvC", "DCAC"), c("FMSYref", "curE"))),
  Hist = FALSE,
  silent = FALSE,
  parallel = TRUE,
  checkMPs = FALSE,
  dropHist = TRUE,
  extended = FALSE
)
```

## Arguments

- MOM:

  A multi-fleet multi-stock operating model (class
  [MOM](https://msetool.openmse.com/reference/MOM-class.md))

- parallel:

  Logical or a named list. Should MPs be run using parallel processing?
  See Details for more information.

- silent:

  Should messages be printed out to the console?

- multiHist:

  An Historical Simulation object (class `multiHist`)

- MPs:

  A matrix of methods (nstock x nfleet) (character string) of class MP

- checkMPs:

  Logical. Check if the specified MPs exist and can be run on
  `SimulatedData`?

- dropHist:

  Logical. Drop the (very large) `multiHist` object from the returned
  `MMSE` object? The `multiHist` object can be (re-)created using
  `SimulateMOM` or kept in `MMSE@multiHist` if `dropHist=FALSE`

- extended:

  Logical. Return extended projection results? if TRUE,
  `MMSE@Misc$extended` is a named list with extended data: FM for
  overall F across fleets
  `[nsim, nstock, n_age, nMP, proyears, nareas]`. `Misc` slot in
  `MMSE@PPD` will also contain StockPars, FleetPars, and ReferencePoints

- Hist:

  Should model stop after historical simulations? Returns a list
  containing all historical data

## Value

Functions return objects of class `MMSE` and `multiHist` \#'

- SimulateMOM - An object of class `multiHist`

- ProjectMOM - An object of class `MMSE`

- multiMSE - An object of class `MMSE`

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

## Functions

- `SimulateMOM()`: Simulate historical dynamics for multi-OM

- `ProjectMOM()`: Run Forward Projections for a `MOM` object

- `multiMSE()`: Run a multi-stock, multi-fleet MSE

## Author

T. Carruthers and A. Hordyk
