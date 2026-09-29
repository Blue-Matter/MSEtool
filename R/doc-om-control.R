#' Operating Model Control Settings
#'
#' `OM@Control` is a named list of optional settings that modify how an
#' [om-class] operating model is simulated and projected. Every setting has
#' a default, so `OM@Control` can be empty.
#'
#' Add or change individual settings with `Control(OM)$Setting <- value`.
#' `Control(OM) <- list(...)` replaces the whole list. Unrecognised names
#' produce a warning in [Simulate()] and are otherwise ignored.
#'
#' @section Simulation settings:
#' Used by [Simulate()]:
#'
#' | Setting | Default | Description |
#' |---|---|---|
#' | `MSYType` | `'Removals'` | `'Removals'` or `'Landings'`. Whether MSY-based and other F reference points are defined in terms of total removals (landings + dead discards) or landings only. Passed as `type` to [CalcMSY()] and [CalcRefPoints()]. |
#' | `RefYears` | `NULL` | Year(s) of biological and fishery parameters used to calculate reference points. `NULL` uses the final historical year. Passed as `Years` to [CalcMSY()] and [CalcRefPoints()]. |
#' | `CorrelatedRecDevs` | `TRUE` | Multi-stock OMs only. Whether projection recruitment deviations are correlated across stocks using the historical covariance; see [GenMultiStockRecDevs()]. |
#' | `HistRel` | `TRUE` | Whether MICE `Relations` are applied in the historical period. |
#' | `BackCalcEffort` | `TRUE` | Whether effort is replaced by the effort implied by realised F when the `maxF` constraint binds. Applies to historical and projection time steps. |
#' | `RefYield` | `list(lastnTS = 5)` | Used when reference yield is calculated (see [SimControl()]). `lastnTS`: number of final projection *years* averaged. |
#' | `DataOM` | `NULL` | Operating model quantities passed to MPs; see the `DataOM` section. |
#'
#' @section Projection settings:
#' Used by [Project()] and [runMSE()]:
#'
#' | Setting | Default | Description |
#' |---|---|---|
#' | `SeasonalAllocationYears` | `5` | Seasonal OMs only. Number of recent historical years pooled to derive the seasonal allocation of annual advice. |
#' | `StockTargeting` | `list(n_recent = 5)` | Multi-stock OMs only. `n_recent`: number of recent historical years used to identify stocks that are no longer targeted; see [GenerateStockTargeting()]. |
#' | `EffortOptim` | `list(lambda_scale = 1, n_recent = 5, maxEval = 500)` | Multi-complex OMs only. Settings for the solver that resolves fleet effort from TACs across complexes. `lambda_scale`: scales the complex-compliance penalty. `n_recent`: number of recent years used to determine which complexes are active. `maxEval`: maximum solver evaluations. |
#' | `ProjectChunks` | `NULL` | Number of simulation chunks projected per MP. `NULL` uses one chunk per parallel worker, or one chunk when `parallel = FALSE`. Results do not depend on the number of chunks. |
#' | `DataOM` | `NULL` | See the `DataOM` section. |
#'
#' @section DataOM:
#' `DataOM` passes true operating model quantities to MPs, for example to
#' build reference or "perfect information" MPs. The selected slots of the
#' [hist-class] object are added to `Data@Misc$DataOM` (a [hist-class]
#' object subset to the current simulation) each time the MP is called.
#'
#' Any slot of [hist-class] except `Data` can be requested, e.g.
#' `Reference`, `Unfished`, `Number`, `Biomass`, `SBiomass`, `Landings`,
#' `Effort`, `FDead`. `DataOM` accepts:
#'
#' - `TRUE`: all slots.
#' - A character vector of slot names, e.g. `c('Biomass', 'Number')`.
#' - A named logical list, e.g. `list(Biomass = TRUE, Number = TRUE)`.
#'   Elements set to `FALSE` are not included.
#'
#' In `OM@Control$DataOM`, elements named after a [hist-class] slot apply to
#' every MP. Any other element name is treated as the name of an MP, and its
#' value (in any of the forms above, or `FALSE`) applies to that MP only:
#'
#' ```r
#' Control(OM)$DataOM <- TRUE                               # all slots, every MP
#' Control(OM)$DataOM <- c('Biomass', 'Number')             # these slots, every MP
#' Control(OM)$DataOM <- list(Reference = TRUE,             # every MP
#'                            myMP = c('Biomass', 'Number'),# myMP only
#'                            MP2  = FALSE)                 # nothing for MP2
#' ```
#'
#' MP functions can also request slots with a `DataOM` attribute, in the
#' same forms:
#'
#' ```r
#' attr(myMP, 'DataOM') <- c('Reference', 'Biomass')
#' ```
#'
#' The slots passed to an MP are:
#' - the MP's entry in `OM@Control$DataOM`, if there is one;
#' - otherwise, the union of the OM-wide slots in `OM@Control$DataOM` and
#'   the MP's `DataOM` attribute.
#'
#' MP-specific entries are matched against the MP names used in
#' [Project()]. [TuneMP()] projects renamed copies of the MP, so use the MP
#' attribute or OM-wide slots for MPs that are tuned.
#'
#' During projections, time-series slots (e.g. `Number`, `Biomass`,
#' `Landings`, `Effort`) include only the time steps before the one the
#' advice applies to (`Data@Misc$AdviceYear`). `OM`, `Unfished`, and
#' `Reference` are passed in full.
#'
#' The reference MPs (see [ReferenceMPs]) set
#' `attr(MP, 'DataOM') <- list(Reference = TRUE)`. An MP that calls one of
#' them internally needs `Reference` too, either from its own `DataOM`
#' attribute or from `OM@Control$DataOM`.
#'
#' @section Internal settings:
#' `Clone`, `CalcCatchAtSizeNeeded`, and `CalcCatchAtSizeCpp` are set
#' internally by [Simulate()] and should not be modified.
#'
#' @seealso [OM()], [Simulate()], [Project()], [SimControl()]
#'
#' @examples
#' \dontrun{
#' OM <- SingleStockOM
#' Control(OM)$MSYType <- 'Landings'
#' Control(OM)$DataOM  <- list(Reference = TRUE)
#'
#' myMP <- function(Data) {
#'   B <- Data@Misc$DataOM@Biomass
#'   refFMSY(Data)
#' }
#' class(myMP) <- 'mp'
#' attr(myMP, 'DataOM') <- 'Biomass'
#'
#' Hist <- Simulate(OM)
#' MSE  <- Project(Hist, MPs = c('refFMSY', 'myMP'))
#' }
#' @name OMControl
NULL
