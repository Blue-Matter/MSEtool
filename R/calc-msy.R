#' Calculate MSY Reference Points
#'
#' Computes maximum sustainable yield (MSY) biological reference points for
#' all complexes in a [hist-class] or [om-class] object by identifying the
#' apical fishing mortality that maximises total yield, accounting for the
#' stock-recruitment relationship. Results are stored in
#' `Hist@Reference@MSY` and the updated [hist-class] object is returned.
#'
#' @param Hist  A [hist-class] or [om-class] object. If an `om` is supplied
#'   it is first converted to a `hist` object.
#' @param Years Integer vector of years for which reference points are
#'   evaluated. Biological and fishery parameters are subset to these years. 
#'   If `NULL` (default), the final
#'   historical year is used.
#' @param type  Character. Whether MSY is defined in terms of total removals
#'   (landings plus dead discards) or landings only. One of `'Removals'`
#'   (default) or `'Landings'`.
#' @param parallel Logical. If `TRUE`, calculates reference points across
#'   simulations in parallel using a `future` plan established by
#'   [SetupParallel()]. Falls back to sequential execution with a warning if
#'   no parallel plan is active. Default `FALSE`.
#' @param silent Logical. If `TRUE`, suppresses progress messages. Default
#'   `FALSE`.
#'
#'
#' @details
#'
#' ## Complexes
#' Reference points are calculated separately for each complex defined in
#' `Hist@OM@Complexes`. Within each complex:
#'
#' - `FMSY` is the single apical fishing mortality maximising total yield
#'   across all stocks in the complex.
#' - Fleet-specific F is distributed according to effort-weighted
#'   catchability and selectivity.
#' - Stock-level quantities (`BMSY`, `SBMSY`, etc.) are reported for each
#'   stock evaluated at the complex-level `FMSY`.
#' 
#' @return An a [refpointsMSY-class] object. 
#'
#' @seealso [CalcPerRecruit()], [CalcSPR0()], [refpointsMSY-class],
#'   [SetupParallel()]
#'
#' @export
CalcMSY <- function(Hist,
                    Years = NULL,
                    type = c('Removals', 'Landings'),
                    parallel = FALSE,
                    silent = FALSE) {

  type <- match.arg(type)
  .CheckClass(Hist, c('om', 'hist'))
  
  if (inherits(Hist, 'om'))
    Hist <- .OM2Hist(Hist, silent=TRUE)

  .CheckClass(Hist, 'hist', 'Hist')
  
  nSeason        <- Hist@OM@Seasons
  RefSeason      <- Hist@OM@RefSeason
  RefEffortYears <- Hist@OM@RefEffortYears

  if (is.null(Years)) {
    Years <- utils::tail(Years(Hist@OM, 'Historical'), 1)
    if (nSeason > 1L) Years <- unique(floor(Years))
  }

  if (is.null(Hist@Reference@SPR0))
    Hist@Reference@SPR0 <- CalcSPR0(Hist, silent=TRUE)

  Hist <- .InitMSYRefPoints(Hist, Years)

  StockNames <- StockNames(Hist)

  complexes <- Complexes(Hist)

  for (i in seq_along(complexes)) {
    Hist <- .CalcRefMSYComplex(Hist,
                               complex_stocks = StockNames[complexes[[i]]],
                               complex_name   = names(complexes)[i],
                               Years          = Years,
                               type           = type,
                               parallel       = parallel,
                               silent         = silent,
                               RefSeason      = RefSeason,
                               RefEffortYears = RefEffortYears)
  }

  if (!silent)
    cli::cli_alert_success("Calculated MSY Reference Points")

  Hist@Reference@MSY
}

.CalcRefMSYComplex <- function(Hist, complex_stocks, complex_name, Years,
                               type, parallel = FALSE, silent = FALSE,
                               RefSeason = NULL, RefEffortYears = NULL) {
  
  StockList <- Hist@OM@Stock[complex_stocks]
  FleetList <- Hist@OM@Fleet[complex_stocks]
  
  nSim          <- nSim(Hist)
  IdenticalHist <- .IdenticalSims(Hist@OM, ignore=c('RecDevInit', 
                                                   'RecDevHist', 
                                                   'RecDevProj',
                                                   'Allocation',
                                                   'Obs',
                                                   'Data'))
  
  SPR0_Full_List <- Array2List(Hist@Reference@SPR0) |>
    .SubsetStock(Stocks = complex_stocks)
  
  logApicalFRange <- log(c(1E-5, Hist@OM@maxF))
  
  MSYRefPoints <- Hist@Reference@MSY
  
  if (IdenticalHist) {
    if (!silent)
      cli::cli_progress_message("Calculating MSY reference points: {.val {complex_name}}")
    
    StockList_sim <- Subset(StockList,      Sims = 1)
    FleetList_sim <- Subset(FleetList,      Sims = 1)
    SPR0_List_sim <- Subset(SPR0_Full_List, Sims = 1)
    
    results_sim1 <- purrr::map(seq_along(Years), \(ts) {
      
      inputs <- .PrepPerRecruitInputs(StockList_sim, FleetList_sim,
                                     SPR0_List_sim, Years[ts],
                                     EffortYears = RefEffortYears, RefSeason = RefSeason)

      opt <- optimize(
        .OptCalcRefMSYSims,
        logApicalFRange,
        inputs       = inputs,
        complex_name = complex_name,
        type         = type,
        option       = 1
      )
      
      .OptCalcRefMSYSims(
        logApicalF   = opt$minimum,
        inputs       = inputs,
        complex_name = complex_name,
        type         = type,
        option       = 2
      )
    })
    
    for (sl in setdiff(slotNames(MSYRefPoints), 'Misc')) {
      arr <- slot(MSYRefPoints, sl)
      if (is.null(arr)) next
      for (ts in seq_along(Years)) {
        val <- slot(results_sim1[[ts]], sl)
        if (!is.null(val))
          ArrayFill(arr) <- val
      }
      slot(MSYRefPoints, sl) <- arr
    }

    Hist@Reference@MSY <- ReduceDims(MSYRefPoints)
    return(Hist)
  }
  
  CalcRefMSY_Sim <- function(sim) {
    StockList_sim  <- .SubsetSim(StockList,      Sims = sim, keep_sim_name = TRUE)
    FleetList_sim  <- .SubsetSim(FleetList,      Sims = sim, keep_sim_name = TRUE)
    SPR0_List_sim  <- .SubsetSim(SPR0_Full_List, Sims = sim, keep_sim_name = TRUE)

    purrr::map(seq_along(Years), \(ts) {

      inputs <- .PrepPerRecruitInputs(StockList   = StockList_sim,
                                      FleetList   = FleetList_sim,
                                      SPR0List    = SPR0_List_sim,
                                      Years       = Years[ts],
                                      EffortYears = RefEffortYears, 
                                      RefSeason   = RefSeason)

      opt <- optimize(
        .OptCalcRefMSYSims,
        logApicalFRange,
        inputs       = inputs,
        complex_name = complex_name,
        type         = type,
        option       = 1
      )

      .OptCalcRefMSYSims(
        logApicalF   = opt$minimum,
        inputs       = inputs,
        complex_name = complex_name,
        type         = type,
        option       = 2
      )
    })
  }

  parallel <- CheckParallel(parallel)

  if (!parallel) {
    if (!silent) {
      id <- cli::cli_progress_bar(
        name   = paste0("Calculating MSY reference points: ", complex_name),
        total  = nSim,
        format = "{cli::pb_name} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} sims | {cli::pb_elapsed}"
      )
    }

    results_by_sim <- purrr::map(seq_len(nSim), \(sim) {
      result <- CalcRefMSY_Sim(sim)
      if (!silent) cli::cli_progress_update(id = id)
      result
    })
  } else {
    if (!silent)
      cli::cli_inform(
        "Calculating MSY reference points: {.val {complex_name}} \\
         ({.val {nSim}} simulation{?s}, parallel) ..."
      )
    CheckPackage('furrr')
    results_by_sim <- furrr::future_map(
      seq_len(nSim),
      CalcRefMSY_Sim,
      .options = furrr::furrr_options(
        globals  = c('StockList', 'FleetList', 'SPR0_Full_List', 'Years',
                     'complex_name', 'type', 'logApicalFRange',
                     'RefSeason', 'RefEffortYears'),
        packages = "MSEtool",
        seed     = 101
      )
    )
  }

  for (sl in setdiff(slotNames(MSYRefPoints), 'Misc')) {
    arr <- slot(MSYRefPoints, sl)
    if (is.null(arr)) next
    
    for (sim in seq_len(nSim)) {
      for (ts in seq_along(Years)) {
        val <- slot(results_by_sim[[sim]][[ts]], sl)
        if (!is.null(val))
          ArrayFill(arr) <- val
      }
    }
      
    slot(MSYRefPoints, sl) <- arr
  }

  Hist@Reference@MSY <- ReduceDims(MSYRefPoints)
  Hist
}

.OptCalcRefMSYSims <- function(logApicalF, inputs, complex_name,
                               type = c('Removals', 'Landings'),
                               option = 1) {
  type <- match.arg(type)
  
  if (length(logApicalF) > 1) {
    cli::cli_alert_danger(
      '{.var logApicalF} must be length 1. Using first value {.val {logApicalF[1]}}')
    logApicalF <- logApicalF[1]
  }
  apicalF <- exp(logApicalF)
  
  PerRecruit <- .CalcPerRecruitF(
    apicalF                   = apicalF,
    StockFleetAllocation      = inputs$StockFleetAllocation,
    NaturalMortalityList      = inputs$NaturalMortalityList,
    PlusGroupList             = inputs$PlusGroupList,
    MaturityList              = inputs$MaturityList,
    SemelparousList           = inputs$SemelparousList,
    WeightList                = inputs$WeightList,
    SpawnTimeFracList         = inputs$SpawnTimeFracList,
    SPFrom                    = inputs$SPFrom,
    SPR0List                  = inputs$SPR0List,
    FecundityList             = inputs$FecundityList,
    WeightFleetRetainedList   = inputs$WeightFleetRetainedList,
    WeightFleetSelectedList   = inputs$WeightFleetSelectedList,
    SelectivityFleetList      = inputs$SelectivityFleetList,
    RetentionFleetList        = inputs$RetentionFleetList,
    DiscardMortalityFleetList = inputs$DiscardMortalityFleetList,
    FleetNames                = inputs$FleetNames,
    Years                     = inputs$Years,
    nSeason                   = inputs$nSeason,
    SeasonalWeightsList       = inputs$SeasonalWeightsList,
    CalendarYears             = inputs$CalendarYears,
    RefSeason                 = inputs$RefSeason,
    NPR0List                  = inputs$NPR0List,
    NPR0_SPList               = inputs$NPR0_SPList
  )


  if (option == 1) {
    Eq <- .CalcEquilibriumInternal(PerRecruit, inputs)
    Removals <- Eq@Removals |> DropDimension("F")
    Landings <- Eq@Landings |> DropDimension("F")
  
    if (type == 'Removals') {
      Catch <- SumOverStock(Removals)
    } else {
      Catch <- SumOverStock(Landings)
    }
    return(-Catch)
  }
  
  Eq <- .CalcEquilibriumInternal(PerRecruit, inputs)

  Removals <- Eq@Removals |> DropDimension("F")
  Landings <- Eq@Landings |> DropDimension("F")
  Discards <- ArraySubtract(Removals, Landings)

  # Report annual apical F for seasonal models (sum of seasonal rates at the
  # apical age); fall back to the raw scalar for annual models.
  fmsy_val <- if (!is.null(PerRecruit@Misc$F_annual_apical)) {
    PerRecruit@Misc$F_annual_apical[[1L]]   # nCalYears=1 for MSY; [Sim] vector
  } else {
    apicalF
  }
  FMSY <- array(fmsy_val, dim(Eq@Biomass |> DropDimension("F")),
                dimnames = dimnames(Eq@Biomass |> DropDimension("F"))) |>
    DropDimension('Stock', warn = FALSE) |>
    AddDimension('Stock', complex_name, pos = 2)

  MSYRefPoints             <- new("refpointsMSY")
  MSYRefPoints@FMSY        <- FMSY
  MSYRefPoints@BMSY        <- Eq@Biomass     |> DropDimension("F")
  MSYRefPoints@SBMSY       <- Eq@SBiomass    |> DropDimension("F")
  MSYRefPoints@SPMSY       <- Eq@SProduction |> DropDimension("F")
  MSYRefPoints@SPRMSY      <- Eq@SPR         |> DropDimension("F") |> .Aperm(c('Sim', 'Stock', 'Year'))
  MSYRefPoints@MSYLandings <- Landings
  MSYRefPoints@MSYDiscards <- Discards
  MSYRefPoints
}
