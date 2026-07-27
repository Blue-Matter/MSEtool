#' Calculate Reference Yield
#'
#' Internal function to calculate reference yields in terms of either 
#' `Landings` and/or `Removals`.
#'
#' Reference yield is calculated as the highest yield (in units of `Units`) 
#' summed across all fleets for a given fixed F policy over the entire 
#' projection period. 
#'
#' @param Hist `hist` class object containing historical fishery dynamics
#' @param type Character vector; one or both of `Landings` and `Removals`
#' @param Units Character; either `Biomass` or `Number`
#' @param parallel Logical; if `TRUE`, optimizes reference yield across
#'   simulations in parallel using a `future` plan established by
#'   [SetupParallel()]. Default `FALSE`.
#' @param silent Logical; if `TRUE`, suppress progress bars
#'
#' @return Updated `Hist` object with reference yields stored in
#'   `Hist@Reference$RefLandings` and/or `Hist@Reference$RefRemovals`
#'
#' @name calc-ref-yield
#'
#' @keywords internal
.CalcRefYield <- function(Hist,
                         type   = c('Landings', 'Removals'),
                         Units  = c("Biomass", 'Number'),
                         parallel = FALSE,
                         silent = FALSE) {

  type  <- match.arg(type, c('Landings', 'Removals'), several.ok=TRUE)
  Units <- match.arg(Units, c("Biomass", 'Number'))
  
  HistYears  <- Years(Hist,'H')
  ProjYears  <- Years(Hist,'P')
  AllYears   <- c(HistYears, ProjYears)
  nSim       <- Hist@OM@nSim
  StockNames <- StockNames(Hist)
  nStock     <- length(StockNames)
  nFleet     <- nFleet(Hist)
  
  # Extend Year dimensions to include ProjYears
  Proj          <- Hist
  Proj@OM@Stock <- Extend(Proj@OM@Stock, Years=AllYears)
  Proj@OM@Fleet <- Extend(Proj@OM@Fleet, Years=AllYears)
  Proj@Misc     <- Extend(Proj@Misc, Years=AllYears)

  for (sl in setdiff(slotNames('timeseries'), c('Misc', 'Distribution')))
    slot(Proj,sl) <- Extend(slot(Proj,sl), Years=AllYears, default=0)
  Proj@Distribution <- Extend(Proj@Distribution, Years=AllYears, default=NA_real_)
  
  ProjYearInd <- match(ProjYears, AllYears)
  nSeason     <- Hist@OM@Seasons

  lastHistIdx     <- ProjYearInd[1] - 1L
  firstLastYearIdx <- lastHistIdx - nSeason + 1L
  LastHistEffort  <- Proj@Effort[, firstLastYearIdx:lastHistIdx, , drop = FALSE]

  parallel <- CheckParallel(parallel)

  # List length nSim, each with a Hist object with 1 sim

  for (t in type) {

    RefYield <- if (parallel) {
      CheckPackage('furrr')
      furrr::future_map(
        seq_len(nSim), .CalcRefYieldSim,
        Proj = Proj, HistYears = HistYears, ProjYears = ProjYears,
        ProjYearInd = ProjYearInd, nFleet = nFleet, Units = Units,
        t = t, LastHistEffort = LastHistEffort,
        .options = furrr::furrr_options(
          globals  = c('Proj', 'HistYears', 'ProjYears', 'ProjYearInd',
                       'nFleet', 'Units', 't', 'LastHistEffort'),
          packages = "MSEtool",
          seed     = 101
        )
      )
    } else {
      if (!silent)
        cli::cli_progress_bar(format = "Calculating Reference {.val {t}} {cli::pb_bar} {cli::pb_percent}",  total = nSim)

      out <- lapply(seq_len(nSim), function(sim) {
        val <- .CalcRefYieldSim(sim, Proj, HistYears, ProjYears, ProjYearInd,
                                nFleet, Units, t, LastHistEffort)
        if (!silent) cli::cli_progress_update()
        val
      })

      if (!silent) cli::cli_progress_done()
      out
    }

    # Convert list of vectors to array sim × stock
    RefYield <- List2Array(RefYield, "Sim", "Stock")[1,,, drop=FALSE] |> abind::adrop(1) |> t()
    dimnames(RefYield)[['Stock']] <- StockNames
    slot(Hist@Reference, t) <- RefYield

  } # end type=c('Landings', 'Removals') loop

  if (!silent) cli::cli_alert_success("Calculated Reference {.val {type}}")
  Hist
}

# Optimises the F scalar for a single simulation (and single `type`), then
# returns the yield at that optimum - the per-sim unit of work .CalcRefYield()
# maps (sequentially or via furrr) over. Slices `Proj` down to this sim once
# via .SliceSim() (R/subset.R) before the repeated .CalcFisheryDynamics()
# probe calls inside optimize().
.CalcRefYieldSim <- function(sim, Proj, HistYears, ProjYears, ProjYearInd,
                             nFleet, Units, t, LastHistEffort) {

  baseEffort <- LastHistEffort[sim,, , drop = FALSE]
  ProjSim <- .SliceSim(Proj, sim, .DynamicsProbeSlots)

  DoOpt <- optimize(f = function(logScalar) {
    .OptRefYield(logScalar,
                Proj = ProjSim,
                sim = 1L,
                HistYears = HistYears,
                ProjYears = ProjYears,
                ProjYearInd = ProjYearInd,
                nFleet = nFleet,
                Units = Units,
                type = t,
                baseEffort = baseEffort,
                debug = 0,
                opt = 1)
  }, interval = log(c(1e-5, 10)))

  .OptRefYield(DoOpt$minimum,
              Proj = ProjSim,
              sim = 1L,
              HistYears = HistYears,
              ProjYears = ProjYears,
              ProjYearInd = ProjYearInd,
              nFleet = nFleet,
              Units = Units,
              type = t,
              baseEffort = baseEffort,
              debug = 0,
              opt = 2)
}
